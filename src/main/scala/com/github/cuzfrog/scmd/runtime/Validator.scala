package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.language.reflectiveCalls

import ScmdUtils._

private object Validator {
  /**
    * Convert string value to typed value and validate it with previously provided function.
    *
    * @param valueNode   the evaluated node to be validated.
    * @param cs          the corresponding ContextSnapshot taken when the node is anchored.
    * @param valiFuncOpt an option of client defined validation function.
    * @tparam T the value type retained by macro,
    *           it plays an important role as the info to which type the arg should be converted.
    * @return the validated and converted typed value(s).
    */
  @throws[ArgValidationException]("When type conversion failed or validation function throws an exception.")
  def validateValueNode
  [T: ClassTag : ArgTypeEvidence](valueNode: ValueNode[T],
                                  cs: ContextSnapshot,
                                  valiFuncOpt: Option[Function1[_, Unit]]): Seq[T] = {
    checkType(valueNode)
    val typedValues = valueNode.value.map(v => verify(v, valueNode, cs)).toList

    valiFuncOpt.foreach { valiFunc =>
      try {
        if (valueNode.isVariable) valiFunc.asInstanceOf[List[T] => Unit].apply(typedValues)
        else typedValues.foreach(valiFunc.asInstanceOf[T => Unit].apply)
      } catch {
        case e: Exception =>
          throw ArgValidationException(s"Arg ${cs.rudeArg} to ${valueNode.entity.name}"
            + s" failed validation: ${e.getMessage}", cs, Some(e))
      }
    }
    typedValues
  }

  @throws[ArgValidationException]("When type conversion failed or validation function throws an exception.")
  def validatePropNode
  [T: ClassTag : ArgTypeEvidence](propNode: PropNode[T],
                                  cs: ContextSnapshot,
                                  valiFuncOpt: Option[Function1[_, Unit]]): Seq[(String, T)] = {
    checkType(propNode)
    val typedValues = propNode.value.map { case (k, v) => (k, verify(v, propNode, cs)) }.toList
    valiFuncOpt.foreach { valiFunc =>
      try {
        valiFunc.asInstanceOf[List[(String, T)] => Unit].apply(typedValues)
      } catch {
        case e: Exception =>
          throw ArgValidationException(s"Arg ${cs.rudeArg} to ${propNode.entity.name}"
            + s" failed validation: ${e.getMessage}", cs, Some(e))
      }
    }
    typedValues
  }

  private def checkType[T: ClassTag](n: {def tpe: ClassTag[_]}): Unit =
    if (implicitly[ClassTag[T]] != n.tpe)
      throw new AssertionError(s"Type of demanded value is different from node's")

  private def verify[T: ArgTypeEvidence : ClassTag](v: String, node: Node, cs: ContextSnapshot): T =
    try {implicitly[ArgTypeEvidence[T]].verify(v)}
    catch {
      case e: Exception =>
        throw ArgValidationException(s"For Arg ${cs.rudeArg} to ${node.entity.originalName}"
          + s", value:$v does not cohere to expected type[${implicitly[ClassTag[T]]}]", cs, Some(e))
    }

  /**
    * Do high level validation on parsed results against mutual limitations defined in an argTree.
    *
    * @param argTree       the arguments shape definition, containing the mutual limitation info,
    *                      which is defined by the client via tree def DSL
    *                      and retained and delivered by macros.
    * @param parsedResults evaluated values with their context snapshot.
    *                      ValueNodes among result returned by ArgParser.
    * @return the same parsedResults that has passed the validation.
    */
  @throws[ArgValidationException]("when one of mutual limitations has been violated.")
  def mutualLimitationValidate(argTree: ArgTree,
                               parsedResults: Seq[(Node, ContextSnapshot)]): Seq[(Node, ContextSnapshot)] = {

    val globalValueNodes = parsedResults.collect { case (n: ValueNode[_], cs) => (n, cs) }

    argTree.globalLimitations.foreach { tree => resolveLimitation(globalValueNodes, tree) }
    //--------------- global validation complete. ---------------------

    val grouped = globalValueNodes.groupBy { case (n, _) => n.locateToCmdNode(argTree) }
    grouped.foreach { case (cmdNode, valueNodes) =>
      cmdNode.limitations.foreach { tree => resolveLimitation(valueNodes, tree) }
    }
    //--------------- per-cmd validation complete. ---------------------
    parsedResults
  }
  private def resolveLimitation(valueNodes: Seq[(Node, ContextSnapshot)],
                                limitationTree: LimitationTree): Unit = {
    val argSymbols = valueNodes.map(_._1.entity.symbol)
    valueNodes.foreach { case (node, cs) =>
      val exclusions = limitationTree.findExclusions(node.entity.symbol)
      val conflicts = exclusions.intersect(argSymbols)
      if (conflicts.nonEmpty) throw ArgValidationException(
        s"${conflicts.map(e => s"'${e.name}'").mkString(",")} " +
          s"cannot be input together with '${node.entity.name}'.", cs
      )

      val dependencies = limitationTree.findDependencies(node.entity.symbol)
      val deficits = dependencies.filter(!argSymbols.contains(_))
      if (deficits.nonEmpty) throw ArgValidationException(
        s"${deficits.map(e => s"'${e.name}'").mkString(",")}" +
          s" should be used with '${node.entity.originalName}'.", cs
      )
    }
  }

  private implicit class OptNodeOps(in: ValueNode[_]) {
    def locateToCmdNode(argTree: ArgTree): CmdNode = recLocate(argTree.toTopNode) match {
      case Some(cmdNode) => cmdNode
      case None => throw new AssertionError(s"ValueArg ${in.entity.name}'s parent Cmd symbol: ${in.parent} cannot be found in arg tree.")
    }

    private def recLocate(cmdNode: CmdNode): Option[CmdNode] = {
      if (in.parent == cmdNode.entity.symbol) Some(cmdNode)
      else cmdNode.subCmdEntry.children.find(n => recLocate(n).nonEmpty)
    }
  }

  implicit class LimitationTreeOps(in: LimitationTree) {
    def findExclusions(name: scala.Symbol): Seq[scala.Symbol] =
      recFindLimitation(name, Limitation.MExclusive, Nil)
    def findDependencies(name: scala.Symbol): Seq[scala.Symbol] =
      recFindLimitation(name, Limitation.MDependent, Nil)

    private def recFindLimitation(name: scala.Symbol,
                                  relation: MutualLimitation,
                                  accOppositeSide: Seq[LimitationTree]): Seq[scala.Symbol] = {
      in match {
        case branch: LimitationBranch =>
          if (branch.relation == relation) {
            val leftProjection = recFindLimitation(name, relation, branch.right +: accOppositeSide)
            val rightProjection = recFindLimitation(name, relation, branch.left +: accOppositeSide)
            leftProjection ++ rightProjection
          } else {
            val leftProjection = recFindLimitation(name, relation, accOppositeSide)
            val rightProjection = recFindLimitation(name, relation, accOppositeSide)
            leftProjection ++ rightProjection
          }
        case leaf: LimitationLeaf =>
          if (leaf.name == name) accOppositeSide.flatMap(_.convertTo[List[scala.Symbol]])
          else Nil
      }
    }
  }
}
