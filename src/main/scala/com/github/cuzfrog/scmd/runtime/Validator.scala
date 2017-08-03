package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.Limitation

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
                        parsedResults: Seq[(ValueNode[_], ContextSnapshot)]): Unit = {
    val acc: ArrayBuffer[scala.Symbol] = ArrayBuffer.empty //use a accumulator for better performance

    argTree.globalLimitations.foreach {
      case (Limitation.MExclusive, group) =>
        parsedResults.foreach { case (node, cs) =>
          group.find(_ == node.entity.symbol).foreach(_ => acc += node.entity.symbol)
          if (acc.length > 1) throw ArgParseException(
            s"${acc.map(_.name).mkString(",")} cannot be input together.", cs
          )
        }
      case (Limitation.MDependent, group) =>
        val resultSymbols = parsedResults.map { case (n, _) => n.entity.symbol }
        parsedResults.find { case (n, _) => group.contains(n.entity.symbol) }
          .foreach { case (foundNode, cs) =>
            val absent = group.collect { case symbol if !resultSymbols.contains(symbol) => symbol }
            if (absent.nonEmpty) throw ArgParseException(
              s"${absent.map(_.name).mkString(",")}" +
                s" should be used with ${foundNode.entity.originalName}.", cs
            )
          }
    }
    acc.clear() //global validation complete.

    ???
  }

}
