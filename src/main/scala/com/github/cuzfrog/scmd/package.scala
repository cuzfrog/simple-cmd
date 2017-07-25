package com.github.cuzfrog

package object scmd {

  def cmdDef(description: String = ""): Command = DummyCommand

  def paramDef[T](description: String = "",
                  isMandatory: Boolean = false,
                  default: => T = Empty): Parameter[T] = DummyParameter

  def optDef[T](abbr: String = "",
                description: String = "",
                default: => T = Empty): OptionArg[T] = DummyOptionArt

  def appDef(name: String,
             shortDescription: String = "",
             fullDescription: String = "",
             version: String = "",
             license: String = "",
             author: String = ""): Unit = ()

  def appDefCustom(item: (String, String)*): Unit = ()

  def validation[T](param: Parameter[T])(f: T => Unit): Unit = ()
  def validation[T](opt: OptionArg[T])(f: T => Unit): Unit = ()

  //def validationMulti[A](args: A, f: A => Boolean): Unit = ()

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
