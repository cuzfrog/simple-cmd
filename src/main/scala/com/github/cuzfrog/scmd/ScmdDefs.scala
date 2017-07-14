package com.github.cuzfrog.scmd


trait ScmdDefs {

  def cmdDef(description: String = ""): Command = DummyCommand

  def paraDef[T](description: String = "", isMandatory: Boolean = false) = DummyParameter

  def optDef[T](name: String = "", description: String = "") = DummyOptionArg

  private implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}
