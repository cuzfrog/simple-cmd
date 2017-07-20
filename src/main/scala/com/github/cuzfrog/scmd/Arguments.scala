package com.github.cuzfrog.scmd

/**
  * Exposed to macros for basic validation, so need to put in this package.
  */
private sealed trait Argument[+A] {
  def name: String
  def description: Option[String]
}

private
case class Command(name: String,
                   description: Option[String]) extends Argument[Nothing]

private
case class CommandEntry(name: String,
                        description: Option[String] = None,
                        //subCmds: immutable.Seq[Command],
                        isMandatory: Boolean = Defaults.isMandatory) extends Argument[Nothing]

private
case class Parameter[+T](name: String,
                         description: Option[String] = None,
                         isMandatory: Boolean = Defaults.isMandatory,
                         default: Option[T] = None) extends Argument[T]

private
case class OptionArg[+T](name: String,
                         abbr: Option[String] = None,
                         description: Option[String] = None,
                         isMandatory: Boolean = Defaults.isMandatory,
                         default: Option[T] = None) extends Argument[T]