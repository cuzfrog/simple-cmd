package com.github.cuzfrog.scmd.macros

import scala.meta._
import scala.collection.immutable

private case class TermValidation(term: Term , argName: String, pos: Position)

private object TermValidation {
  def collectValidation(stats: immutable.Seq[Stat]): immutable.Seq[TermValidation] = {
    val validations = stats zip stats.map(_.pos) collect {
      case (q"validation[$tpe]($param)$stat", pos) =>
        val argName = param match {
          case q"$_.$n" => n
          case n => n
        }
        TermValidation(q"validation[$tpe]($param)$stat", argName.syntax, pos)
    }
    validations
  }
}
