package com.github.cuzfrog

package object scmd{
  def cmdDef(description: String = ""): Command = DummyCommand

  def paraDef[T](description: String = "",
                 isMandatory: Boolean = false): Parameter[Nothing] = DummyParameter

  def optDef[T](name: String = "", description: String = ""): OptionArg[Nothing] = DummyOptionArg

  private object DummyCommand extends Command("", None)
  private object DummyParameter extends Parameter[Nothing]("")
  private object DummyOptionArg extends OptionArg[Nothing]("")

  private implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}
