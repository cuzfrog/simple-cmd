package com.github.cuzfrog.scmd

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
  this: Argument[T] =>
}

sealed trait SingleValue[+T] extends ArgValue[T] {
  this: Argument[T] =>
  def value: Option[T]
  def default: Option[T]
}

sealed trait VariableValue[+T] extends ArgValue[T] {
  this: Argument[T] =>
  def value: Seq[T]
  def default: Seq[T]
}

sealed trait Mandatory

private[scmd] object DummyCommand extends Command("")
private[scmd] object DummyParameter extends Parameter("")
private[scmd] object DummyOptionArt extends OptionArg("")

private object Command {
  implicit def toValue(in: Command): Boolean = in.met
}


private object ValueArgument {

  implicit def mergeValue
  [T, V <: ArgValue[T], A <: ValueArgument[T]]: CanMerge[A with V, Seq[T]] =
    (a: A with V, stuff: Seq[T]) => {
      val result = a match {
        case arg: Parameter[T@unchecked] with SingleValue[T@unchecked] =>
          createParam[T](a.name, a.description, arg.isMandatory, stuff.headOption, arg.default)
        case arg: Parameter[T@unchecked] with VariableValue[T@unchecked] =>
          createParam[T](a.name, a.description, arg.isMandatory, stuff, arg.default)
        case arg: OptionArg[T@unchecked] with SingleValue[T@unchecked] =>
          createOpt[T](a.name, arg.abbr, a.description, arg.isMandatory, stuff.headOption, arg.default)
        case arg: OptionArg[T@unchecked] with VariableValue[T@unchecked] =>
          createOpt[T](a.name, arg.abbr, a.description, arg.isMandatory, stuff, arg.default)
      }
      result.asInstanceOf[A with V]
    }

  implicit def mixValueTrait
  [T, V <: ArgValue[T], A <: ValueArgument[T]]: CanMix[A, V] =
    (a: A, stuff: V) => {
      val result = stuff match {
        case s: SingleValue[T@unchecked] => a match {
          case arg: Parameter[T@unchecked] =>
            createParam[T](a.name, a.description, arg.isMandatory, s.value, s.default)
          case arg: OptionArg[T@unchecked] =>
            createOpt[T](a.name, arg.abbr, a.description, arg.isMandatory, s.value, s.default)
        }
        case m: VariableValue[T@unchecked] => a match {
          case arg: Parameter[T@unchecked] =>
            createParam[T](a.name, a.description, arg.isMandatory, m.value, m.default)
          case arg: OptionArg[T@unchecked] =>
            createOpt[T](a.name, arg.abbr, a.description, arg.isMandatory, m.value, m.default)
        }
      }
      result.asInstanceOf[A with V]
    }

  private def createParam[T](name: String, description: Option[String],
                             isMandatory: Boolean, value: Option[T], default: Option[T]) = {
    new Parameter[T](name = name, description = description, isMandatory = isMandatory)
      with SingleValue[T] {
      override def value: Option[T] = value
      override def default: Option[T] = default
    }
  }
  private def createParam[T](name: String, description: Option[String],
                             isMandatory: Boolean, value: Seq[T], default: Seq[T]) = {
    new Parameter[T](name = name, description = description, isMandatory = isMandatory)
      with VariableValue[T] {
      override def value: Seq[T] = value
      override def default: Seq[T] = default
    }
  }
  private def createOpt[T](name: String, abbr: Option[String], description: Option[String],
                           isMandatory: Boolean, value: Option[T], default: Option[T]) = {
    new OptionArg[T](name = name, abbr = abbr, description = description, isMandatory = isMandatory)
      with SingleValue[T] {
      override def value: Option[T] = value
      override def default: Option[T] = default
    }
  }
  private def createOpt[T](name: String, abbr: Option[String], description: Option[String],
                           isMandatory: Boolean, value: Seq[T], default: Seq[T]) = {
    new OptionArg[T](name = name, abbr = abbr, description = description, isMandatory = isMandatory)
      with VariableValue[T] {
      override def value: Seq[T] = value
      override def default: Seq[T] = default
    }
  }
}