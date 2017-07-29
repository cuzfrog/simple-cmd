package com.github.cuzfrog.scmd

/**
  * Provide direct implicit conversion against Arguments.
  * <br><br>
  * This gives client succinct syntax to use scmd Argument as their value directly. e.g.:
  * Implicit conversion is generally discouraged. Use it within limited scope and with caution.
  *
  */
object ScmdValueImplicitConversion {
  /*
   * S = SingleValue, M = Mandatory, V = VariableValue
   */

  implicit def cmd2Value(in: Command): Boolean = in.met
  implicit def paramS2value[T](in: Parameter[T] with SingleValue[T]): Option[T] = in.value
  implicit def paramSM2value[T](in: Parameter[T] with SingleValue[T] with Mandatory): T = in.value.get
  implicit def paramV2value[T](in: Parameter[T] with VariableValue[T]): Seq[T] = in.value
  //           paramVM same as above
  implicit def optS2value[T](in: OptionArg[T] with SingleValue[T]): Option[T] = in.value
  implicit def optSM2value[T](in: OptionArg[T] with SingleValue[T] with Mandatory): T = in.value.get
  implicit def optV2value[T](in: OptionArg[T] with VariableValue[T]): Seq[T] = in.value
  //           optVM same as above
}

sealed trait LowLevelImplicitsOfScmdValueConverter {
  sealed trait ValueConvertible[T, A <: Argument[T], R] {
    def convert(a: A): R
  }

  implicit def paramS2value[T]: ValueConvertible[T, Parameter[T] with SingleValue[T], Option[T]] =
    new ValueConvertible[T, Parameter[T] with SingleValue[T], Option[T]] {
      override def convert(a: Parameter[T] with SingleValue[T]): Option[T] = a.value
    }
}


object ScmdValueConverter extends LowLevelImplicitsOfScmdValueConverter {
  
  implicit final class ValueConversionOps
  [T, A <: Argument[T], R](arg: A)(implicit ev: ValueConvertible[T, A, R]) {
    def value: R = ev.convert(arg)
  }

  implicit val cmd2Value: ValueConvertible[Nothing, Command, Boolean] =
    new ValueConvertible[Nothing, Command, Boolean] {
      override def convert(a: Command): Boolean = a.met
    }

  implicit def paramSM2value[T]: ValueConvertible
    [T, Parameter[T] with SingleValue[T] with Mandatory, T] =
    new ValueConvertible[T, Parameter[T] with SingleValue[T] with Mandatory, T] {
      override def convert(a: Parameter[T] with SingleValue[T] with Mandatory): T = a.value.get
    }

  //  implicit final class ValueConversionOptionOps
  //  [T, A <: Argument[T] with SingleValue[T], R](arg: A)(implicit ev: ValueConvertible[T, A, R]) {
  //    def valueOption: R = ev.convert(arg)
  //  }
}