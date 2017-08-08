package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.ScmdSafeValueConverter.PropsV

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
  implicit def paramS2value[T](in: Parameter[T] with SingleValue[T]): Option[T] = in.v
  implicit def paramSM2value[T](in: Parameter[T] with SingleValue[T] with Mandatory): T =
    in.v.get
  implicit def paramSD2value[T](in: Parameter[T] with SingleValue[T] with WithDefault): T =
    in.v.getOrElse(in.default.get)
  implicit def paramV2value[T](in: Parameter[T] with VariableValue[T]): Seq[T] =
    if (in.v.nonEmpty) in.v else in.default
  //           paramVM same as above
  implicit def optS2value[T](in: OptionArg[T] with SingleValue[T]): Option[T] = in.v
  implicit def optSM2value[T](in: OptionArg[T] with SingleValue[T] with Mandatory): T =
    in.v.get
  implicit def optSD2value[T](in: OptionArg[T] with SingleValue[T] with WithDefault): T =
    in.v.get
  implicit def optV2value[T](in: OptionArg[T] with VariableValue[T]): Seq[T] =
    if (in.v.nonEmpty) in.v else in.default
  //           optVM same as above
}

sealed abstract class AbstractScmdValueConverter {
  implicit class Cmd2ValueOps(in: Command) {
    def value: Boolean = in.met
    def met: Boolean = in.met
  }

  implicit class SingleValueMOps[T](a: Argument[T] with SingleValue[T] with Mandatory) {
    def value: T = a.v.getOrElse(throwIfEmptyMandatory(a))
  }

  implicit class SingleValueDOps[T](a: Argument[T] with SingleValue[T] with WithDefault) {
    def value: T = a.v.getOrElse(a.default.getOrElse(throwIfEmptyDefault(a)))
  }

  protected type PropsV[T] = PropertyArg[T] with VariableValue[(String, T)]
  implicit class PropsOps[T](propertyArg: PropsV[T]) {
    def apply(key: String): Option[T] =
      propertyArg.v.collectFirst { case (k, v) if k == key => v }
  }

  protected def throwIfEmptyDefault(arg: Argument[_]): Nothing =
    throw new IllegalArgumentException(s"Default value empty for ${arg.originalName}")
  protected def throwIfEmptyMandatory(arg: Argument[_]): Nothing =
    throw new IllegalArgumentException(s"Mandatory value empty for ${arg.originalName}")
}

/**
  * Provide explicit method to get value of Argument.
  */
object ScmdValueConverter extends AbstractScmdValueConverter {
  implicit class SingleValueOps[T](a: Argument[T] with SingleValue[T]) {
    def value: Option[T] = a.v
  }

  implicit class VariableValueOps[T](a: Argument[T] with VariableValue[T]) {
    def value: Seq[T] = a.v
  }

  implicit class PropsValueOps[T](propertyArg: PropsV[T]) {
    def value: Seq[(String, T)] = propertyArg.v
  }
}

/**
  * Provide explicit method to get value of Argument.
  */
object ScmdSafeValueConverter extends AbstractScmdValueConverter {
  implicit class SingleValueOps[T](a: Argument[T] with SingleValue[T]) {
    def valueOpt: Option[T] = a.v
  }

  implicit class VariableValueOps[T](a: Argument[T] with VariableValue[T]) {
    def valueSeq: Seq[T] = a.v
  }

  implicit class PropsValueOps[T](propertyArg: PropsV[T]) {
    def valueSeq: Seq[(String, T)] = propertyArg.v
  }
}