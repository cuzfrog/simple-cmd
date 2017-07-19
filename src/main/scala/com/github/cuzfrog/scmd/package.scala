package com.github.cuzfrog

package object scmd {

  def cmdDef(description: String = ""): Command = DummyCommand

  def paramDef[T](description: String = "",
                  isMandatory: Boolean = false,
                  default: => T = Empty): Parameter[T] = DummyParameter

  def optDef[T](abbr: String = "",
                description: String = "",
                default: => T = Empty): OptionArg[T] = DummyOptionArg

  def appDef(name: String,
             shortDescription: Option[String] = None,
             fullDescription: Option[String] = None,
             version: Option[String] = None,
             license: Option[String] = None,
             author: Option[String] = None,
             custom: Map[String, String] = Map.empty): ProgramInfo = DummyProgramInfo


  private object DummyCommand extends Command("", None)
  //private object DefaultEntry extends CommandEntry("", None, true)
  private object DummyParameter extends Parameter[Nothing]("")
  private object DummyOptionArg extends OptionArg[Nothing]("")
  private object DummyProgramInfo extends ProgramInfo("")

  //private implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)

  /**
    * A placeholder to make parameters optional.
    * For client's simplicity, `Option` is not used.
    */
  private def Empty[T]: T =
    throw new IllegalArgumentException("Empty default value called. See scmd.Empty.")

  private[scmd] trait CanFormPrettyString[A] {
    def mkPrettyString(a: A): String
  }
  private[scmd] implicit class PrettyStringBuildOps[A: CanFormPrettyString](a: A) {
    def prettyString: String = implicitly[CanFormPrettyString[A]].mkPrettyString(a)
  }
}
