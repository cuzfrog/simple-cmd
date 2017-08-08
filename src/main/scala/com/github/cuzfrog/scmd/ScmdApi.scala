package com.github.cuzfrog.scmd

import DummyArgument._

/**
  * Client api to define scmd args.
  *
  * @define isMandatory
  * If the arg is required from cmd-line.
  * This affects styles of <strong>route-api</strong> and <strong>value-extraction-api</strong>.
  * <br>
  * see [[com.github.cuzfrog.scmd.ScmdRouteDSL]]<br>
  * see [[com.github.cuzfrog.scmd.ScmdValueConverter]]<br>
  * see [[com.github.cuzfrog.scmd.ScmdValueImplicitConversion]]<br>
  * @define DEFAULT
  * Mandatory param cannot have default value.
  * For `Boolean` value, the "default of the default" value is false.<br>
  * @define TYPE_T
  * Type of argument value.
  */
trait ScmdApi {
  // --------------------- Def methods ----------------------
  /**
    * Define a command. Name of the named-parameter cannot be omitted.
    * e.g.{{{
    * val command = cmdDef(description = "this is a command.")
    * //the "description" cannot be omitted.
    * }}}
    */
  final def cmdDef(description: String = ""): Command = DummyCommand

  /**
    * Define a parameter with single value.
    * <br><br>
    * $DEFAULT
    *
    * @param isMandatory $isMandatory<br>
    * @tparam T $TYPE_T<br>
    */
  final def paramDef[T](description: String = "",
                        isMandatory: Boolean = false,
                        default: => T = Empty): Parameter[T] with SingleValue[T] = DummyParameterS
  /**
    * Define a variable parameter.
    * <br><br>
    * $DEFAULT
    *
    * @param isMandatory $isMandatory<br>
    * @tparam T $TYPE_T<br>
    */
  final def paramDefVariable[T](description: String = "",
                                isMandatory: Boolean = false,
                                default: => T = Empty): Parameter[T] with VariableValue[T] = DummyParameterV
  /**
    * Define an opt with single value.
    * <br><br>
    * $DEFAULT
    *
    * @param abbr abbreviation of this opt.
    *             Abbr may consist of multiple letters, though this is not recommended.
    * @param isMandatory $isMandatory<br>
    * @tparam T $TYPE_T<br>
    */
  final def optDef[T](abbr: String = "",
                      description: String = "",
                      isMandatory: Boolean = false,
                      default: => T = Empty): OptionArg[T] with SingleValue[T] = DummyOptionArgS
  /**
    * Define a variable opt.
    * <br><br>
    * A variable opt may have multiple value. It can be specified multiple times in the cmd-line args:
    * {{{
    * --variable-opt=value1 --variable-opt value2 ...etc
    * //value will be stored in Seq
    * }}}
    * $DEFAULT
    *
    * @param abbr abbreviation of this opt.
    *             Abbr may consist of multiple letters, though this is not recommended.
    * @param isMandatory $isMandatory<br>
    * @tparam T $TYPE_T<br>
    */
  final def optDefVariable[T](abbr: String = "",
                              description: String = "",
                              isMandatory: Boolean = false,
                              default: => T = Empty): OptionArg[T] with VariableValue[T] = DummyOptionArgV

  /**
    * Define a properties.
    * <br><br>
    * $DEFAULT
    *
    * @param flag
    * @param description
    * @param default
    * @tparam T
    * @return
    */
  final def propDef[T](flag: String = "",
                       description: String = "",
                       default: => Seq[(String, T)] = Empty): PropertyArg[T] with VariableValue[(String, T)] = DummyProp

  final def priorDef(alias: Seq[String] = Nil,
                     description: String = "",
                     matchName: Boolean = false): PriorArg = DummyPriorArg

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

object DummyApi {
  final def cmdDef: Command = DummyCommand
  final def priorDef: PriorArg = DummyPriorArg

  final def ParameterSingleValue[T]: Parameter[T] with SingleValue[T] = DummyParameterS
  final def ParameterSingleValueMandatory[T]: Parameter[T] with SingleValue[T] with Mandatory = DummyParameterSM
  final def ParameterSingleValueDefault[T]: Parameter[T] with SingleValue[T] with WithDefault = DummyParameterSD
  final def ParameterVariableValue[T]: Parameter[T] with VariableValue[T] = DummyParameterV
  final def ParameterVariableValueMandatory[T]: Parameter[T] with VariableValue[T] with Mandatory = DummyParameterVM
  final def ParameterVariableValueDefault[T]: Parameter[T] with VariableValue[T] with WithDefault = DummyParameterVD

  final def OptionArgSingleValue[T]: OptionArg[T] with SingleValue[T] = DummyOptionArgS
  final def OptionArgSingleValueMandatory[T]: OptionArg[T] with SingleValue[T] with Mandatory = DummyOptionArgSM
  final def OptionArgSingleValueDefault[T]: OptionArg[T] with SingleValue[T] with WithDefault = DummyOptionArgSD
  final def OptionArgVariableValue[T]: OptionArg[T] with VariableValue[T] = DummyOptionArgV
  final def OptionArgVariableValueMandatory[T]: OptionArg[T] with VariableValue[T] with Mandatory = DummyOptionArgVM
  final def OptionArgVariableValueDefault[T]: OptionArg[T] with VariableValue[T] with WithDefault = DummyOptionArgVD

  final def PropertyArgVariableValue[T]: PropertyArg[T] with VariableValue[(String, T)] = DummyProp
  final def PropertyArgVariableValueDefault[T]: PropertyArg[T] with VariableValue[(String, T)] with WithDefault = DummyPropD
}