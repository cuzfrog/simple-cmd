package com.github.cuzfrog.scmd.macros

import scala.meta._

private case class TermValidation(term: Term, pos: Position)

private object TermValidation {
  def collectValidation(stats: Seq[Stat]): TermValidation = {
    stats zip stats.map(_.pos) collect {
      case (q"validation[$tpe]($param)($func)", pos) =>
    }


    ???
  }
}
