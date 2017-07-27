package com.github.cuzfrog.scmd

import scala.meta._

package object macros {

  import Constants._

  /** Typeclass that define A to a single Term. */
  private[macros] trait Definable[A] {
    /** Define A to a single Term. */
    def defnTerm(a: A): Term
  }

  private[macros] implicit class DefineTermOps[A: Definable](a: A) {
    /** Define A to a single Term. */
    def defnTerm: Term = {
      val definable = implicitly[Definable[A]]
      definable.defnTerm(a)
    }
  }

  private[macros] implicit val definableCommand: Definable[Command] = (a: Command) => {
    val name = Lit.String(a.name)
    val description = description2term(a.description)
    q"Command($TERM_NAME = $name,$TERM_DESCRIPTION = $description)"
  }

  private[macros] trait OptionTermEv[T] {
    def makeTerm(a: T): Term
  }
  private[macros] implicit def definableOption[T]: Definable[Option[T]] = {
    case Some(s: String) => q"Option(${Lit.String(s)})"
    case Some(s: Boolean) => q"Option(${Lit.Boolean(s)})"
    case None => q"None"
    case Some(s) => throw new IllegalArgumentException(s"Unsupported Option type.")
  }

  private[macros] implicit val definableBoolean: Definable[Boolean] = (a: Boolean) => Lit.Boolean(a)
  private[macros] implicit val definableString: Definable[String] = (a: String) => Lit.String(a)
  //  implicit val definableCommandEntry: Definable[CommandEntry] = (a: CommandEntry) => {
  //    val name = Lit.String(a.name)
  //    val description = description2term(a.description)
  //    val isMandatory = Lit.Boolean(a.isMandatory)
  //    val subCmds = a.subCmds.map(_.defnTerm)
  //    q"""CommandEntry($TERM_NAME = $name,
  //                       $TERM_DESCRIPTION = $description,
  //                       $TERM_SUBCMDS = $TERM_immutable.Seq(..$subCmds),
  //                       $TERM_IS_MANDATORY = $isMandatory)"""
  //  }

  private def description2term(in: Option[String]): Term = in match {
    case Some(dscr) => q"Option(${Lit.String(dscr)})"
    case None => q"None"
  }
}
