package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

/**
  * Exposed to macros for basic validation, so need to put in this package.
  */
sealed trait Argument[+T] {
  def name: String
  def description: Option[String]
}


case class
Command private[scmd](name: String,
                      description: Option[String] = None,
                      private[scmd] val met: Boolean = false) extends Argument[Nothing]


case class
CommandEntry private[scmd](name: String,
                           description: Option[String] = None,
                           //subCmds: immutable.Seq[Command],
                           isMandatory: Boolean = Defaults.isMandatory) extends Argument[Nothing]

sealed trait ValueArgument[+T] extends Argument[T]

case class
Parameter[+T] private[scmd](name: String,
                            description: Option[String] = None,
                            isMandatory: Boolean = Defaults.isMandatory) extends ValueArgument[T] {
}


case class
OptionArg[+T] private[scmd](name: String,
                            abbr: Option[String] = None,
                            description: Option[String] = None,
                            isMandatory: Boolean = Defaults.isMandatory) extends ValueArgument[T] {

}

sealed trait ArgValue[+T] {
  def isVariable: Boolean
}

sealed trait SingleValue[+T] extends ArgValue[T] {
  def value: Option[T] = None
  def default: Option[T] = None
  def isVariable: Boolean = false
}
sealed trait VariableValue[+T] extends ArgValue[T] {
  def value: Seq[T] = Nil
  def default: Seq[T] = Nil
  def isVariable: Boolean = true
}

sealed trait Mandatory

private[scmd] object DummyCommand extends Command("")
private[scmd] object DummyParameterS extends Parameter("") with SingleValue[Nothing]
private[scmd] object DummyParameterV extends Parameter("") with VariableValue[Nothing]
private[scmd] object DummyOptionArgS extends OptionArg("") with SingleValue[Nothing]
private[scmd] object DummyOptionArgV extends OptionArg("") with VariableValue[Nothing]

private object Command {
  implicit def toValue(in: Command): Boolean = in.met
}

private object Parameter {
  implicit def mergeValue[T]: CanMerge[Parameter[T], Seq[T]] =
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

  implicit def mixValueTrait
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


  private def createParam[T](name: String, description: Option[String],
                             isMandatory: Boolean, _value: Option[T], _default: Option[T]) = {
    new Parameter[T](name = name, description = description, isMandatory = isMandatory)
      with SingleValue[T] {
      override def value: Option[T] = _value
      override def default: Option[T] = _default
    }
  }
  private def createParam[T](name: String, description: Option[String],
                             isMandatory: Boolean, _value: Seq[T], _default: Seq[T]) = {
    new Parameter[T](name = name, description = description, isMandatory = isMandatory)
      with VariableValue[T] {
      override def value: Seq[T] = _value
      override def default: Seq[T] = _default
    }
  }
}

private object OptionArg {
  implicit def mergeValue[T]: CanMerge[OptionArg[T], Seq[T]] =
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

  implicit def mixValueTrait
  [T, V <: ArgValue[T], A <: ValueArgument[T]]: CanMix[OptionArg[T], V] =
    (a: OptionArg[T], stuff: V) => {
      val result = stuff match {
        case s: SingleValue[T@unchecked] =>
          createOpt[T](a.name, a.abbr, a.description, a.isMandatory, s.value, s.default)

        case m: VariableValue[T@unchecked] =>
          createOpt[T](a.name, a.abbr, a.description, a.isMandatory, m.value, m.default)
      }
      result.asInstanceOf[OptionArg[T] with V]
    }


  private def createOpt[T](name: String, abbr: Option[String], description: Option[String],
                           isMandatory: Boolean, _value: Option[T], _default: Option[T]) = {
    new OptionArg[T](name = name, abbr = abbr, description = description, isMandatory = isMandatory)
      with SingleValue[T] {
      override def value: Option[T] = _value
      override def default: Option[T] = _default
    }
  }
  private def createOpt[T](name: String, abbr: Option[String], description: Option[String],
                           isMandatory: Boolean, _value: Seq[T], _default: Seq[T]) = {
    new OptionArg[T](name = name, abbr = abbr, description = description, isMandatory = isMandatory)
      with VariableValue[T] {
      override def value: Seq[T] = _value
      override def default: Seq[T] = _default
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
  private[scmd] def single[T](_default: Option[T]): ArgValue[T] = new SingleValue[T] {
    override def default: Option[T] = _default
    override def value: Option[T] = None
  }
  private[scmd] def variable[T](_default: Seq[T]): ArgValue[T] = new VariableValue[T] {
    override def default: Seq[T] = _default
    override def value: Seq[T] = Nil
  }
}