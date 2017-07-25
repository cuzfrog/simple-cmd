package com.github.cuzfrog.scmd

/**
  * Exposed to macros for basic validation, so need to put in this package.
  */
sealed trait Argument[+A] {
  def name: String
  def description: Option[String]
}


case class Command(name: String,
                   description: Option[String] = None) extends Argument[Nothing]


case class CommandEntry(name: String,
                        description: Option[String] = None,
                        //subCmds: immutable.Seq[Command],
                        isMandatory: Boolean = Defaults.isMandatory) extends Argument[Nothing]


case class Parameter[+T](name: String,
                         description: Option[String] = None,
                         isMandatory: Boolean = Defaults.isMandatory,
                         default: Option[T] = None) extends Argument[T]


case class OptionArg[+T](name: String,
                         abbr: Option[String] = None,
                         description: Option[String] = None,
                         isMandatory: Boolean = Defaults.isMandatory,
                         default: Option[T] = None) extends Argument[T]

private[scmd] object DummyCommand extends Command("")
private[scmd] object DummyParameter extends Parameter[Nothing]("")
private[scmd] object DummyOptionArt extends OptionArg[Nothing]("")