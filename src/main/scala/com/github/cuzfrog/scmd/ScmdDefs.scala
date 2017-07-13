package com.github.cuzfrog.scmd


trait ScmdDefs {

  def cmdDef(description: String = ""): Command =
    Command(name = "", description = description)

  def paraDef[T](description: String = "", isMandatory: Boolean = false) =
    Parameter(description = description, isMandatory = isMandatory)

  def optDef[T](name: String = "", description: String = "") =
    OptionArg(name = name, description = description)

  private implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}
