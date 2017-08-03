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

//todo: add default value fallback
/**
  * Provide explicit method to get value of Argument.
  */
object ScmdValueConverter extends LowLevelImplicitsOfScmdValueConverter {

  implicit final class ValueConversionOps
  [T, A, R](arg: A)(implicit ev: ValueConvertible[T, A, R], ev1: A <:< Argument[T]) {
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

  implicit def optSM2value[T]: ValueConvertible
    [T, OptionArg[T] with SingleValue[T] with Mandatory, T] =
    new ValueConvertible[T, OptionArg[T] with SingleValue[T] with Mandatory, T] {
      override def convert(a: OptionArg[T] with SingleValue[T] with Mandatory): T = a.value.get
    }
}

sealed trait LowLevelImplicitsOfScmdValueConverter {
  sealed trait ValueConvertible[T, -A, +R] {
    def convert(a: A): R
  }

  implicit def paramS2value[T]: ValueConvertible[T, Parameter[T] with SingleValue[T], Option[T]] =
    new ValueConvertible[T, Parameter[T] with SingleValue[T], Option[T]] {
      override def convert(a: Parameter[T] with SingleValue[T]): Option[T] = a.value
    }

  implicit def paramV2value[T]: ValueConvertible[T, Parameter[T] with VariableValue[T], Seq[T]] =
    new ValueConvertible[T, Parameter[T] with VariableValue[T], Seq[T]] {
      override def convert(a: Parameter[T] with VariableValue[T]): Seq[T] = a.value
    }

  implicit def optS2value[T]: ValueConvertible[T, OptionArg[T] with SingleValue[T], Option[T]] =
    new ValueConvertible[T, OptionArg[T] with SingleValue[T], Option[T]] {
      override def convert(a: OptionArg[T] with SingleValue[T]): Option[T] = a.value
    }

  implicit def optV2value[T]: ValueConvertible[T, OptionArg[T] with VariableValue[T], Seq[T]] =
    new ValueConvertible[T, OptionArg[T] with VariableValue[T], Seq[T]] {
      override def convert(a: OptionArg[T] with VariableValue[T]): Seq[T] = a.value
    }
}