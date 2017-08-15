package com.github.cuzfrog.scmd

import ScmdUtils._

/**
  * Provide direct implicit conversion against Arguments.
  * <br><br>
  * This gives client succinct syntax to use scmd Argument as their value directly. e.g.:
  * Implicit conversion is generally discouraged. Use it within limited scope and with caution.
  *
  */
object ScmdValueImplicitConversion extends AbstractScmdValueConverter
  with LowLevelImplicitForScmdValueImplicitConversion {
  /*
   * S = SingleValue, V = VariableValue, M = Mandatory, D = WithDefault
   */

  implicit def cmd2Value(in: Command): Boolean = in.met

  implicit def paramSM2value[T](in: ValueArgument[T] with SingleValue[T] with Mandatory): T =
    in.v.getOrElse(throwIfEmptyMandatory(in))
  implicit def paramSD2value[T](in: ValueArgument[T] with SingleValue[T] with WithDefault): T =
    in.v.getOrElse(in.default.getOrElse(throwIfEmptyDefault(in)))
  implicit def paramV2value[T](in: ValueArgument[T] with VariableValue[T]): Seq[T] =
    if (in.v.nonEmpty) in.v else in.default
  //           paramVM same as above

  implicit def props2value[T](in: PropsV[T]): Seq[(String, T)] = in.v
  implicit def prior2value(in: PriorArg): Option[Symbol] = in.met
}

sealed trait LowLevelImplicitForScmdValueImplicitConversion {
  implicit def paramS2value[T](in: ValueArgument[T] with SingleValue[T]): Option[T] = in.v

  implicit def boolValueArgument2value
  (in: ValueArgument[Boolean] with SingleValue[Boolean]): Boolean = {
    in.v.getOrElse(in.default.getOrElse(throwIfEmptyDefault(in)))
  }
}

sealed abstract class AbstractScmdValueConverter {
  implicit class ValueConverterCmdOps(in: Command) {
    def value: Boolean = in.met
  }

  protected type PropsV[T] = PropertyArg[T] with VariableValue[(String, T)]
  implicit class ValueConverterPropsOps[T](propertyArg: PropsV[T]) {
    def apply(key: String): Option[T] =
      propertyArg.v.collectFirst { case (k, v) if k == key => v }
  }
}

/**
  * Provide explicit method to get value of Argument.
  */
object ScmdValueConverter
  extends AbstractScmdValueConverter with LowLevelImplicitForScmdValueConverter {
  implicit class ValueConverterSingleValueMOps[T](a: ValueArgument[T] with SingleValue[T] with Mandatory) {
    def value: T = a.v.getOrElse(throwIfEmptyMandatory(a))
  }

  implicit class ValueConverterSingleValueDOps[T]
  (a: ValueArgument[T] with SingleValue[T] with WithDefault) {
    def value: T = a.v.getOrElse(a.default.getOrElse(throwIfEmptyDefault(a)))
  }

  implicit class ValueConverterVariableValueOps[T](a: ValueArgument[T] with VariableValue[T]) {
    def value: Seq[T] = a.v
  }

  implicit class ValueConverterPropsValueOps[T](propertyArg: PropsV[T]) {
    def value: Seq[(String, T)] = propertyArg.v
  }

  implicit class ValueConverterPriorValueOps(priorArg: PriorArg) {
    def value: Option[Symbol] = priorArg.met
  }
}

sealed trait LowLevelImplicitForScmdValueConverter {
  implicit class ValueConverterBooleanSingleValueOps
  (a: ValueArgument[Boolean] with SingleValue[Boolean]) {
    def value: Boolean = a.v.getOrElse(a.default.getOrElse(throwIfEmptyMandatory(a)))
  }

  implicit class ValueConverterSingleValueOps[T](a: ValueArgument[T] with SingleValue[T]) {
    def value: Option[T] = a.v
  }
}

/**
  * Provide explicit method to get value of Argument.
  */
@deprecated("Not necessary", "dev")
private object ScmdSafeValueConverter extends AbstractScmdValueConverter {
  implicit class SingleValueMOps[T](a: ValueArgument[T] with SingleValue[T] with Mandatory) {
    def value: T = a.v.getOrElse(throwIfEmptyMandatory(a))
  }

  implicit class SingleValueDOps[T](a: ValueArgument[T] with SingleValue[T] with WithDefault) {
    def valueWithDefault: T = a.v.getOrElse(a.default.getOrElse(throwIfEmptyDefault(a)))
  }

  implicit class SingleValueOps[T](a: ValueArgument[T] with SingleValue[T]) {
    def valueOpt: Option[T] = a.v
  }

  implicit class VariableValueOps[T](a: ValueArgument[T] with VariableValue[T]) {
    def valueSeq: Seq[T] = a.v
  }

  implicit class PropsValueOps[T](propertyArg: PropsV[T]) {
    def valueSeq: Seq[(String, T)] = propertyArg.v
  }

  implicit class ValueConverterPriorValueOps(priorArg: PriorArg) {
    def valueOpt: Option[Symbol] = priorArg.met
  }
}