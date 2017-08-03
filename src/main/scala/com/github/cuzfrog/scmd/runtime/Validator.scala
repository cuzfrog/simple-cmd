package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.{Limitation, MutualLimitation}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

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
  @throws[ArgParseException]("When type conversion failed or validation function throws an exception.")
  def validate[T: ClassTag : ArgTypeEvidence](valueNode: ValueNode[T],
                                              cs: ContextSnapshot,
                                              valiFuncOpt: Option[Function1[_, Unit]]): Seq[T] = {
    val tpe = implicitly[ClassTag[T]]
    val typeEvidence = implicitly[ArgTypeEvidence[T]]
    if (tpe != valueNode.tpe)
      throw new AssertionError(s"Type of demanded value is different from node's")

    val typedValue = valueNode.value.map { str =>
      try {typeEvidence.verify(str)}
      catch {
        case e: Exception =>
          throw new ArgParseException(s"Arg ${cs.rudeArg} to ${valueNode.entity.name}"
            + s" of value:$str does not confirm expected type[$tpe]", cs, Some(e))
      }
    }.toList

    valiFuncOpt.foreach { valiFunc =>
      try {
        if (valueNode.isVariable) valiFunc.asInstanceOf[List[T] => Unit].apply(typedValue)
        else typedValue.foreach(valiFunc.asInstanceOf[T => Unit].apply)
      } catch {
        case e: Exception =>
          throw new ArgParseException(s"Arg ${cs.rudeArg} to ${valueNode.entity.name}"
            + s" failed validation: ${e.getMessage}", cs, Some(e))
      }
    }
    typedValue
  }

  /**
    * Do high level validation on parsed results against mutual limitations defined in an argTree.
    *
    * @param argTree       the arguments shape definition, containing the mutual limitation info,
    *                      which is defined by the client via tree def DSL
    *                      and retained and delivered by macros.
    * @param parsedResults evaluated values with their context snapshot.
    *                      ValueNodes among result returned by ArgParser.
    */
  @throws[ArgParseException]("when one of mutual limitations has been violated.")
  def highLevelValidate(argTree: ArgTree,
                        parsedResults: Seq[(Node, ContextSnapshot)]): Unit = {
    val acc: ArrayBuffer[scala.Symbol] = ArrayBuffer.empty //use a accumulator for better performance
    val globalValueNodes = parsedResults.collect { case (n: ValueNode[_], cs) => (n, cs) }

    def resolveMExclusive(valueNodes: Seq[(Node, ContextSnapshot)], group: Seq[Symbol]) = {
      valueNodes.foreach { case (node, cs) =>
        group.find(_ == node.entity.symbol).foreach(_ => acc += node.entity.symbol)
        if (acc.length > 1) throw ArgParseException(
          s"${acc.map(_.name).mkString(",")} cannot be input together.", cs
        )
      }
    }

    def resolveMDependent(valueNodes: Seq[(Node, ContextSnapshot)], group: Seq[Symbol]) = {
      val resultSymbols = valueNodes.map { case (n, _) => n.entity.symbol }
      valueNodes.find { case (n, _) => group.contains(n.entity.symbol) }
        .foreach { case (foundNode, cs) =>
          val absent = group.collect { case symbol if !resultSymbols.contains(symbol) => symbol }
          if (absent.nonEmpty) throw ArgParseException(
            s"${absent.map(_.name).mkString(",")}" +
              s" should be used with ${foundNode.entity.originalName}.", cs
          )
        }
    }

    argTree.globalLimitations.foreach {
      case (Limitation.MExclusive, group) => resolveMExclusive(globalValueNodes, group)
      case (Limitation.MDependent, group) => resolveMDependent(globalValueNodes, group)
    }
    acc.clear() //--------------- global validation complete. ---------------------

    val grouped = globalValueNodes.groupBy { case (n, _) => n.locateToCmdNode(argTree) }
    grouped.foreach{ case (cmdNode, valueNodes) =>
      cmdNode.limitations.foreach {
        case (Limitation.MExclusive, group) => resolveMExclusive(valueNodes, group)
        case (Limitation.MDependent, group) => resolveMDependent(valueNodes, group)
      }
    }
    //--------------- per-cmd validation complete. ---------------------
  }


  private implicit class OptNodeOps(in: ValueNode[_]) {
    def locateToCmdNode(argTree: ArgTree): CmdNode = recLocate(argTree.toTopNode) match {
      case Some(cmdNode) => cmdNode
      case None => throw new AssertionError(s"Cmd symbol: ${in.parent} cannot be found in arg tree.")
    }

    private def recLocate(cmdNode: CmdNode): Option[CmdNode] = {
      if (in.parent == cmdNode.entity.symbol) Some(cmdNode)
      else cmdNode.subCmdEntry.children.find(n => recLocate(n).nonEmpty)
    }
  }

}
