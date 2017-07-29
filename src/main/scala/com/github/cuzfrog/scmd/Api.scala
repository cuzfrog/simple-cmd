package com.github.cuzfrog.scmd

trait Api {
  // --------------------- Def methods ----------------------
  final def cmdDef(description: String = ""): Command = DummyCommand

  final def paramDef[T](description: String = "",
                        isMandatory: Boolean = false,
                        default: => T = Empty): Parameter[T] with SingleValue[T] = DummyParameterS

  final def paramDefVariable[T](description: String = "",
                                isMandatory: Boolean = false,
                                default: => T = Empty): Parameter[T] with VariableValue[T] = DummyParameterV

  final def optDef[T](abbr: String = "",
                      description: String = "",
                      isMandatory: Boolean = false,
                      default: => T = Empty): OptionArg[T] with SingleValue[T] = DummyOptionArgS

  final def optDefMultiple[T](abbr: String = "",
                              description: String = "",
                              default: => T = Empty): OptionArg[T] with VariableValue[T] = DummyOptionArgV

  final def appDef(name: String,
                   shortDescription: String = "",
                   fullDescription: String = "",
                   version: String = "",
                   license: String = "",
                   author: String = ""): Unit = ()

  def appDefCustom(item: (String, String)*): Unit = ()


  def validation[T](arg: SingleValue[T])(f: T => Unit): T => Unit = f
  def validation[T](arg: VariableValue[T])(f: List[T] => Unit): List[T] => Unit = f

  //def validationMulti[A](args: A, f: A => Boolean): Unit = ()

  //private implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}

object MetaApi {
  final def cmdDef: Command = DummyCommand

  final def ParameterSingleValue[T]: Parameter[T] with SingleValue[T] = DummyParameterS
  final def ParameterSingleValueMandatory[T]: Parameter[T] with SingleValue[T] with Mandatory = DummyParameterSM
  final def ParameterVariableValue[T]: Parameter[T] with VariableValue[T] = DummyParameterV
  final def ParameterVariableValueMandatory[T]: Parameter[T] with VariableValue[T] with Mandatory = DummyParameterVM

  final def OptionArgSingleValue[T]: OptionArg[T] with SingleValue[T] = DummyOptionArgS
  final def OptionArgSingleValueMandatory[T]: OptionArg[T] with SingleValue[T] with Mandatory = DummyOptionArgSM
  final def OptionArgVariableValue[T]: OptionArg[T] with VariableValue[T] = DummyOptionArgV
  final def OptionArgVariableValueMandatory[T]: OptionArg[T] with VariableValue[T] with Mandatory = DummyOptionArgVM
}

