package com.github.cuzfrog.scmd


trait ScmdDefs {

  def cmdDef(name: String = "", description: String = ""): Command = DummyCommand

  def paraDef[T](description: String = "", isMandatory: Boolean = false) = DummyParameter

  //def optDef(name: String = "", isNameLess: Boolean = false,)
}
