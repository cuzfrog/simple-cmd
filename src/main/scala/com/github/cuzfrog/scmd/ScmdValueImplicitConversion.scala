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
   * S = SingleValue, V = VariableValue, M = Mandatory, D = WithDefault
   */

  implicit def cmd2Value(in: Command): Boolean = in.met
  implicit def paramS2value[T](in: Parameter[T] with SingleValue[T]): Option[T] = in.value
  implicit def paramSM2value[T](in: Parameter[T] with SingleValue[T] with Mandatory): T =
    in.value.get
  implicit def paramSD2value[T](in: Parameter[T] with SingleValue[T] with WithDefault): T =
    in.value.getOrElse(in.default.get)
  implicit def paramV2value[T](in: Parameter[T] with VariableValue[T]): Seq[T] =
    if (in.value.nonEmpty) in.value else in.default
  //           paramVM same as above
  implicit def optS2value[T](in: OptionArg[T] with SingleValue[T]): Option[T] = in.value
  implicit def optSM2value[T](in: OptionArg[T] with SingleValue[T] with Mandatory): T =
    in.value.get
  implicit def optSD2value[T](in: OptionArg[T] with SingleValue[T] with WithDefault): T =
    in.value.get
  implicit def optV2value[T](in: OptionArg[T] with VariableValue[T]): Seq[T] =
    if (in.value.nonEmpty) in.value else in.default
  //           optVM same as above
}

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
      override def convert(a: Parameter[T] with SingleValue[T] with Mandatory): T =
        a.value.getOrElse(throwIfEmptyMandatory(a))
    }
  implicit def paramSD2value[T]: ValueConvertible
    [T, Parameter[T] with SingleValue[T] with WithDefault, T] =
    new ValueConvertible[T, Parameter[T] with SingleValue[T] with WithDefault, T] {
      override def convert(a: Parameter[T] with SingleValue[T] with WithDefault): T =
        a.value.getOrElse(a.default.getOrElse(throwIfEmptyDefault(a)))
    }

  implicit def optSM2value[T]: ValueConvertible
    [T, OptionArg[T] with SingleValue[T] with Mandatory, T] =
    new ValueConvertible[T, OptionArg[T] with SingleValue[T] with Mandatory, T] {
      override def convert(a: OptionArg[T] with SingleValue[T] with Mandatory): T =
        a.value.getOrElse(throwIfEmptyMandatory(a))
    }
  implicit def optSD2value[T]: ValueConvertible
    [T, OptionArg[T] with SingleValue[T] with WithDefault, T] =
    new ValueConvertible[T, OptionArg[T] with SingleValue[T] with WithDefault, T] {
      override def convert(a: OptionArg[T] with SingleValue[T] with WithDefault): T =
        a.value.getOrElse(a.default.getOrElse(throwIfEmptyDefault(a)))
    }

  implicit class PropsOps[T](propertyArg: PropsV[T]) {
    def apply(key: String): Option[T] =
      propertyArg.value.collectFirst { case (k, v) if k == key => v }
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

  protected type PropsV[T] = PropertyArg[T] with VariableValue[(String, T)]
  implicit def propsV2value[T]: ValueConvertible[T, PropsV[T], Seq[(String, T)]] =
    new ValueConvertible[T, PropsV[T], Seq[(String, T)]] {
      override def convert(a: PropsV[T]): Seq[(String, T)] =
        if (a.value.nonEmpty) a.value else a.default
    }

  protected def throwIfEmptyDefault(arg: Argument[_]): Nothing =
    throw new AssertionError(s"Default value empty for ${arg.originalName}")
  protected def throwIfEmptyMandatory(arg: Argument[_]): Nothing =
    throw new AssertionError(s"Mandatory value empty for ${arg.originalName}")
}