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


case class
Parameter[+T] private[scmd](name: String,
                            description: Option[String] = None,
                            isMandatory: Boolean = Defaults.isMandatory) extends Argument[T] {
}


case class
OptionArg[+T] private[scmd](name: String,
                            abbr: Option[String] = None,
                            description: Option[String] = None,
                            isMandatory: Boolean = Defaults.isMandatory) extends Argument[T] {

}

sealed trait ArgValue[+T] {
  this: Argument[T] =>
}

sealed trait SingleValue[+T] extends ArgValue[T] {
  this: Argument[T] =>
  def value: Option[T] = None
  def default: Option[T] = None
}

sealed trait VariableValue[+T] extends ArgValue[T] {
  this: Argument[T] =>
  def value: Seq[T] = Seq()
  def default: Seq[T] = Seq()
}

sealed trait Mandatory

private[scmd] object DummyCommand extends Command("")
private[scmd] object DummyParameter extends Parameter("")
private[scmd] object DummyOptionArt extends OptionArg("")

private object Command {
  implicit def toValue(in: Command): Boolean = in.met
}


private object Parameter {
  implicit def fillableWithSingleValue
  [T, V <: ArgValue[T]]: Fillable[Parameter[T] with V, Seq[T]] =
    (a: Parameter[T] with V, stuff: Seq[T]) => {
      val result = a match {
        case s: Parameter[T@unchecked] with SingleValue[T@unchecked] =>
          new Parameter(name = a.name, description = a.description, isMandatory = a.isMandatory)
            with SingleValue[T] {
            override def value: Option[T] = stuff.headOption
            override def default: Option[T] = s.default
          }
        case m: Parameter[T@unchecked] with VariableValue[T@unchecked] =>
          new Parameter(name = a.name, description = a.description, isMandatory = a.isMandatory)
            with VariableValue[T] {
            override def value: Seq[T] = stuff
            override def default: Seq[T] = m.default
          }
      }
      result.asInstanceOf[Parameter[T] with V]
    }
}

private object OptionArg {
  implicit def fillableWithSingleValue
  [T, V <: ArgValue[T]]: Fillable[OptionArg[T] with V, Seq[T]] =
    (a: OptionArg[T] with V, stuff: Seq[T]) => {
      val result = a match {
        case s: OptionArg[T@unchecked] with SingleValue[T@unchecked] =>
          new OptionArg(name = a.name, description = a.description, isMandatory = a.isMandatory)
            with SingleValue[T] {
            override def value: Option[T] = stuff.headOption
            override def default: Option[T] = s.default
          }
        case m: OptionArg[T@unchecked] with VariableValue[T@unchecked] =>
          new OptionArg(name = a.name, description = a.description, isMandatory = a.isMandatory)
            with VariableValue[T] {
            override def value: Seq[T] = stuff
            override def default: Seq[T] = m.default
          }
      }
      result.asInstanceOf[OptionArg[T] with V]
    }
}