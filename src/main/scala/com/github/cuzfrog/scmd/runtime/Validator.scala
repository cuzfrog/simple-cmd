package com.github.cuzfrog.scmd.runtime

import scala.reflect.ClassTag

private object Validator {
  /** Convert string value to typed value and validate it with previously provided function. */
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
}
