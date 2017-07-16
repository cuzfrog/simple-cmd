package com.github.cuzfrog

package object scmd {

  def cmdDef(description: String = ""): Command = DummyCommand

  def paraDef[T](description: String = "",
                 isMandatory: Boolean = false,
                 default: => T = Empty): Parameter[Nothing] = DummyParameter

  def optDef[T](abbr: String = "",
                description: String = "",
                default: => T = Empty): OptionArg[Nothing] = DummyOptionArg

  private[scmd] object DummyCommand extends Command("", None)
  private object DummyParameter extends Parameter[Nothing]("")
  private object DummyOptionArg extends OptionArg[Nothing]("")

  //private implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)

  /**
    * A placeholder to make parameters optional.
    * For client's simplicity, `Option` is not used.
    */
  private def Empty[T]: T =
    throw new IllegalArgumentException("Empty default value called. See scmd.Empty.")
}
