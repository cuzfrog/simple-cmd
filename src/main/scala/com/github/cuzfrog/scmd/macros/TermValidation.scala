package com.github.cuzfrog.scmd.macros

import scala.meta._
import scala.collection.immutable

private case class TermValidation(func: Term.Arg, argName: String, pos: Position)

private object TermValidation {
  def collectValidation(stats: immutable.Seq[Stat]): immutable.Seq[TermValidation] = {
    val validations = stats zip stats.map(_.pos) collect {
      case (q"validation[$tpe]($param)($func)", pos) =>
        val argName = param match {
          case q"$_.$n" => n
          case n => n
        }
        TermValidation(func, argName.syntax, pos)
    }
    validations
  }
}
