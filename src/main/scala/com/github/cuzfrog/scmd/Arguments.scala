package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.internal.ArgCreationMacro

/**
  * Exposed to macros for basic validation, so need to put in this package.
  */
sealed trait Argument[+T] {
  def name: String
  def description: Option[String]
  /** name prepended with hyphen if this is an opt */
  def originalName: String = name
  def symbol: scala.Symbol = scala.Symbol(originalName)
}


sealed case class
Command private[scmd](name: String,
                      description: Option[String] = None,
                      private[scmd] val met: Boolean = false) extends Argument[Nothing]


sealed case class
CommandEntry private[scmd](isMandatory: Boolean = Defaults.isMandatory)

sealed trait ValueArgument[+T] extends Argument[T] {
  def isMandatory: Boolean
}

sealed case class
Parameter[+T] private[scmd](name: String,
                            description: Option[String] = None,
                            isMandatory: Boolean = Defaults.isMandatory) extends ValueArgument[T]

sealed case class
OptionArg[+T] private[scmd](name: String,
                            abbr: Option[String] = None,
                            description: Option[String] = None,
                            isMandatory: Boolean = Defaults.isMandatory) extends ValueArgument[T] {
  val hyphenName: String = OptionArg.camelCase2hyphen(name)
  final override val originalName: String = "--" + hyphenName
}

sealed case class
PropertyArg[+T] private[scmd](name: String,
                              flag: String,
                              description: Option[String] = None) extends Argument[T] {
  final override val originalName: String = "-" + flag
}

/**
  * PriorArgs are special arguments that trigger specific actions,
  * similar to commands, while they can be of any shape.
  * During args parsing, a PriorArg has priority to be matched and must be fully matched.
  * At a time only the first PriorArg will be picked, others dropped.
  * <br><br>
  * Client can define a PriorArg within the scope of a (sub) command.
  * This is handy, e.g. providing scoped help info for each command.
  * <br><br>
  * If a PriorArg starts with a letter, it will only be matched closely after a command.
  * <br><br>
  * Scmd provide basic PriorArg: <strong>help</strong> and <strong>version</strong>.
  * Both have alias with - or -- prepended.
  */
sealed case class
PriorArg private[scmd](name: String,
                       alias: Seq[String] = Nil,
                       matchName: Boolean = Defaults.priorMatchName,
                       description: Option[String] = None,
                       private[scmd] val met: Boolean = false) extends Argument[Nothing]

sealed trait ArgValue[+T] {
  private[scmd] def isVariable: Boolean
}

sealed trait SingleValue[+T] extends ArgValue[T] {
  private[scmd] def value: Option[T] = None
  private[scmd] def default: Option[T] = None
  private[scmd] def isVariable: Boolean = false
}
sealed trait VariableValue[+T] extends ArgValue[T] {
  private[scmd] def value: Seq[T] = Nil
  private[scmd] def default: Seq[T] = Nil
  private[scmd] def isVariable: Boolean = true
}

sealed trait Mandatory
sealed trait WithDefault

//todo: check if adding lazy val will aid performance.
private[scmd] object DummyArgument {
  def DummyCommand: Command = Command("")
  def DummyPriorArg: PriorArg = PriorArg("")

  def DummyParameterS: Parameter[Nothing] with SingleValue[Nothing] = new Parameter("") with SingleValue[Nothing]
  def DummyParameterV: Parameter[Nothing] with VariableValue[Nothing] = new Parameter("") with VariableValue[Nothing]
  def DummyOptionArgS: OptionArg[Nothing] with SingleValue[Nothing] = new OptionArg("") with SingleValue[Nothing]
  def DummyOptionArgV: OptionArg[Nothing] with VariableValue[Nothing] = new OptionArg("") with VariableValue[Nothing]
  def DummyParameterSM: Parameter[Nothing] with SingleValue[Nothing] with Mandatory = new Parameter("") with SingleValue[Nothing] with Mandatory
  def DummyParameterVM: Parameter[Nothing] with VariableValue[Nothing] with Mandatory = new Parameter("") with VariableValue[Nothing] with Mandatory
  def DummyOptionArgSM: OptionArg[Nothing] with SingleValue[Nothing] with Mandatory = new OptionArg("") with SingleValue[Nothing] with Mandatory
  def DummyOptionArgVM: OptionArg[Nothing] with VariableValue[Nothing] with Mandatory = new OptionArg("") with VariableValue[Nothing] with Mandatory
  def DummyParameterSD: Parameter[Nothing] with SingleValue[Nothing] with WithDefault = new Parameter("") with SingleValue[Nothing] with WithDefault
  def DummyParameterVD: Parameter[Nothing] with VariableValue[Nothing] with WithDefault = new Parameter("") with VariableValue[Nothing] with WithDefault
  def DummyOptionArgSD: OptionArg[Nothing] with SingleValue[Nothing] with WithDefault = new OptionArg("") with SingleValue[Nothing] with WithDefault
  def DummyOptionArgVD: OptionArg[Nothing] with VariableValue[Nothing] with WithDefault = new OptionArg("") with VariableValue[Nothing] with WithDefault

  def DummyProp: PropertyArg[Nothing] with VariableValue[Nothing] = new PropertyArg("", "") with VariableValue[Nothing]
  def DummyPropD: PropertyArg[Nothing] with VariableValue[Nothing] with WithDefault = new PropertyArg("", "") with VariableValue[Nothing] with WithDefault
}

private object Command {
  /** Return a top cmd representing app. */
  def topCmd(appName: String): Command = Command(appName, None)
}

private object Parameter {
  private[scmd] implicit def mergeValue[T]: CanMerge[Parameter[T], Seq[T]] =
    (a: Parameter[T], stuff: Seq[T]) => {
      val result = a match {
        case arg: SingleValue[T@unchecked] =>
          createParam[T](a.name, a.description, arg.isMandatory, stuff.headOption, arg.default)
        case arg: VariableValue[T@unchecked] =>
          createParam[T](a.name, a.description, arg.isMandatory, stuff, arg.default)
        case arg: Parameter[T] => throw new AssertionError(s"Nude arg:$arg cannot merge a value.")
      }
      result.asInstanceOf[Parameter[T]]
    }

  private[scmd] implicit def mixValueTrait
  [T, V <: ArgValue[T]]: CanMix[Parameter[T], V] =
    (a: Parameter[T], stuff: V) => {
      val result = stuff match {
        case s: SingleValue[T@unchecked] =>
          createParam[T](a.name, a.description, a.isMandatory, s.value, s.default)
        case m: VariableValue[T@unchecked] =>
          createParam[T](a.name, a.description, a.isMandatory, m.value, m.default)
      }
      result.asInstanceOf[Parameter[T] with V]
    }

  @ArgCreationMacro
  private def createParam[T](name: String, description: Option[String],
                             isMandatory: Boolean, _value: Option[T], _default: Option[T]) = Empty
  @ArgCreationMacro
  private def createParam[T](name: String, description: Option[String],
                             isMandatory: Boolean, _value: Seq[T], _default: Seq[T]) = Empty
}

private object OptionArg {
  private[scmd] implicit def mergeValue[T]: CanMerge[OptionArg[T], Seq[T]] =
    (a: OptionArg[T], stuff: Seq[T]) => {
      val result = a match {
        case arg: SingleValue[T@unchecked] =>
          createOpt[T](a.name, arg.abbr, a.description, arg.isMandatory, stuff.headOption, arg.default)
        case arg: VariableValue[T@unchecked] =>
          createOpt[T](a.name, arg.abbr, a.description, arg.isMandatory, stuff, arg.default)
        case arg: OptionArg[T] => throw new AssertionError(s"Nude arg:$arg cannot merge a value.")
      }
      result.asInstanceOf[OptionArg[T]]
    }

  private[scmd] implicit def mixValueTrait
  [T, V <: ArgValue[T]]: CanMix[OptionArg[T], V] =
    (a: OptionArg[T], stuff: V) => {
      val result = stuff match {
        case s: SingleValue[T@unchecked] =>
          createOpt[T](a.name, a.abbr, a.description, a.isMandatory, s.value, s.default)

        case m: VariableValue[T@unchecked] =>
          createOpt[T](a.name, a.abbr, a.description, a.isMandatory, m.value, m.default)
      }
      result.asInstanceOf[OptionArg[T] with V]
    }

  @ArgCreationMacro
  private def createOpt[T](name: String, abbr: Option[String], description: Option[String],
                           isMandatory: Boolean, _value: Option[T], _default: Option[T]) = Empty
  @ArgCreationMacro
  private def createOpt[T](name: String, abbr: Option[String], description: Option[String],
                           isMandatory: Boolean, _value: Seq[T], _default: Seq[T]) = Empty

  @inline
  private def camelCase2hyphen(camelCase: String): String = {
    var lastUpper: Boolean = true //ignore first char
    camelCase.flatMap { char =>
      if (char.isUpper && !lastUpper) {
        lastUpper = true
        Seq('-', char.toLower)
      }
      else {
        if (char.isLower) lastUpper = false
        Seq(char.toLower)
      }
    }
  }
}

private object PropertyArg {
  private[scmd] implicit def mergeValue[T]: CanMerge[PropertyArg[T], Seq[(String, T)]] =
    (a: PropertyArg[T], stuff: Seq[(String, T)]) => a match {
      case arg: VariableValue[(String, T)@unchecked] =>
        createProp[T](a.name, arg.flag, a.description, stuff, arg.default)
      case arg: PropertyArg[T] => throw new AssertionError(s"Nude arg:$arg cannot merge a value.")
    }


  private[scmd] implicit def mixValueTrait[T]: CanMix[PropertyArg[T], VariableValue[(String, T)]] =
    (a: PropertyArg[T], stuff: VariableValue[(String, T)]) => {
      createProp[T](a.name, a.flag, a.description, stuff.value, stuff.default)
    }

  private def createProp[T](name: String, flag: String, description: Option[String],
                            _value: Seq[(String, T)], _default: Seq[(String, T)]) = {
    if (_default.nonEmpty) {
      new PropertyArg[T](name = name, flag = flag, description = description)
        with VariableValue[(String, T)] with WithDefault {
        override def value: Seq[(String, T)] = _value
        override def default: Seq[(String, T)] = _default
      }
    } else {
      new PropertyArg[T](name = name, flag = flag, description = description)
        with VariableValue[(String, T)] {
        override def value: Seq[(String, T)] = _value
        override def default: Seq[(String, T)] = _default
      }
    }
  }
}

private object ValueArgument {
  implicit def mergeValue
  [T, V <: ArgValue[T]]: CanMerge[ValueArgument[T], Seq[T]] =
    (a: ValueArgument[T], stuff: Seq[T]) => {
      val result = a match {
        case p: Parameter[T] => merge(p, stuff)
        case o: OptionArg[T] => merge(o, stuff)
      }
      result.asInstanceOf[ValueArgument[T] with V]
    }
}

private object ArgValue {
  private[scmd] def single[T](_default: Option[T]): SingleValue[T] = new SingleValue[T] {
    override def default: Option[T] = _default
    override def value: Option[T] = None
  }
  private[scmd] def variable[T](_default: Seq[T]): VariableValue[T] = new VariableValue[T] {
    override def default: Seq[T] = _default
    override def value: Seq[T] = Nil
  }
}