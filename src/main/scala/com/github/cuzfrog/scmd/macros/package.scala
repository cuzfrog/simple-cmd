package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.ScmdUtils.CanFormPrettyString

import scala.meta._
import scala.reflect.ClassTag
import scala.collection.immutable

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

  private[macros] implicit def definableOption[T]: Definable[Option[T]] = {
    case Some(s: String) => q"Option(${Lit.String(s)})"
    case Some(s: Boolean) => q"Option(${Lit.Boolean(s)})"
    case Some(s: Term.Arg) => q"Option($s)"
    case None => q"None"
    case Some(s) => throw new IllegalArgumentException(s"Unsupported Option type.")
  }

  private[macros] implicit def definableSeq[T: ClassTag]: Definable[immutable.Seq[T]] =
    (a: immutable.Seq[T]) => {
      implicitly[ClassTag[T]].runtimeClass match {
        case rc if rc == classOf[String] =>
          val args = a.asInstanceOf[immutable.Seq[String]].map(s => s.defnTerm)
          q"_root_.scala.collection.immutable.Seq(..$args)"
        case rc => throw new IllegalArgumentException(s"Unsupported Seq type.")
      }
    }

  private[macros] implicit val definableBoolean: Definable[Boolean] = (a: Boolean) => Lit.Boolean(a)
  private[macros] implicit val definableString: Definable[String] = (a: String) => Lit.String(a)

  private def description2term(in: Option[String]): Term = in match {
    case Some(dscr) => q"Option(${Lit.String(dscr)})"
    case None => q"None"
  }

  private[macros] implicit val symbolSeqPrettyString: CanFormPrettyString[Seq[scala.Symbol]] =
    (a: Seq[scala.Symbol]) => a.map(s => s"'${s.name}'").mkString(",")
}
