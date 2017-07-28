package com.github.cuzfrog.scmd

/**
  * Provide direct implicit conversion against Arguments.
  * <br><br>
  * This gives client succinct syntax to use scmd Argument as their value directly. e.g.:
  * Implicit conversion is generally discouraged. Use it within limited scope and with caution.
  *
  */
object ScmdValueImplicitConversion {
  implicit def cmd2Value(in: Command): Boolean = in.met
  implicit def paramS2value[T](in: Parameter[T] with SingleValue[T]): Option[T] = in.value
  implicit def paramSM2value[T](in: Parameter[T] with SingleValue[T] with Mandatory): T = in.value.get
  implicit def paramV2value[T](in: Parameter[T] with VariableValue[T]): Seq[T] = in.value
  implicit def optS2value[T](in: OptionArg[T] with SingleValue[T]): Option[T] = in.value
  implicit def optSM2value[T](in: OptionArg[T] with SingleValue[T] with Mandatory): T = in.value.get
  implicit def optV2value[T](in: OptionArg[T] with VariableValue[T]): Seq[T] = in.value
}
