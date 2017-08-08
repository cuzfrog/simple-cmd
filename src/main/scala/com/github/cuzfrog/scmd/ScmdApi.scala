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
trait ScmdDefApi {
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
    * @param abbr        abbreviation of this opt.
    *                    Abbr may consist of multiple letters, though this is not recommended.
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
    * @param abbr        abbreviation of this opt.
    *                    Abbr may consist of multiple letters, though this is not recommended.
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
    * Properties are global, which are read from anywhere of the cmd-lin args.
    * e.g. {{{-Dkey=value}}}.
    * <br><br>
    * $DEFAULT
    *
    * @param flag the flag of this properties.
    * @tparam T $TYPE_T<br>
    */
  final def propDef[T](flag: String = "",
                       description: String = "",
                       default: => Seq[(String, T)] = Empty): PropertyArg[T] with VariableValue[(String, T)] = DummyProp

  /**
    * Define a prior argument.
    * <br><br>
    * A prior argument has priority to be matched.
    * Once they appear on cmd-lin args, the first of them will be picked, and the whole parsing ends.
    * <br><br>
    * Prior args are defined globally, but when matching, one is scoped to its left-closest command.
    * e.g.{{{
    * >app command1 prior1 param1 command2 prior2
    * //prior1 will be matched and scoped to command1, param1 and args on its right will be ignored.
    * >app command1 param1 command2 prior2
    * //prior2 will be matched and scoped to command2
    * }}}
    * Scmd provides built-in prior args: "help" and "version"
    *
    * @see [[com.github.cuzfrog.scmd.Argument.BuiltInArgs]]
    * @param alias     a prior arg can have other names. e.g. "-help", "--help"
    * @param matchName if the val name of this prior arg def will be matched.
    */
  final def priorDef(alias: Seq[String] = Nil,
                     description: String = "",
                     matchName: Boolean = false): PriorArg = DummyPriorArg

  /**
    * Define app with basic info.
    */
  final def appDef(name: String,
                   shortDescription: String = "",
                   fullDescription: String = "",
                   version: String = "",
                   license: String = "",
                   author: String = ""): Unit = ()

  /**
    * Define app with custom info.
    * <br><br>
    * If basic info exists, basic info and custom info will be merged,
    * with basic info preceding.
    * If custom info contains a basic info key, the basic info will take
    * the place defined in custom info.
    */
  def appDefCustom(item: (String, String)*): Unit = ()

  implicit final class MandatoryOps
  [T, A](in: A)
           (implicit ev1: A <:< ValueArgument[T] with ArgValue[T]) {
    def mandatory: A with Mandatory = ???
  }
}

/**
  * @define VALIDATION_F
  * the validation statement.
  * Note: statement returns a Unit, and should use exceptions to indicate a failure.
  */
trait ScmdValidationApi {
  /**
    * Define a validation against an argument.
    *
    * @param arg the argument to validate.
    * @param f   $VALIDATION_F <br>
    * @tparam T the type of the value of the argument, inferred and delivered to validation function.
    */
  def validation[T](arg: SingleValue[T])(f: T => Unit): T => Unit = f
  /**
    * Define a validation against an argument.
    *
    * @param arg the argument to validate.
    * @param f   $VALIDATION_F <br>
    * @tparam T the type of the value of the argument, inferred and delivered to validation function.
    */
  def validation[T](arg: VariableValue[T])(f: List[T] => Unit): List[T] => Unit = f

  //def validationMulti[A](args: A, f: A => Boolean): Unit = ()
}

/** Api managed by macro, serving type transformation. */
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