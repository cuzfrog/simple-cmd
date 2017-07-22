package com.github.cuzfrog.scmd.macros

import scala.meta._

private case class TermValidation(func: Stat, argName: String, pos: Position)

private object TermValidation {
  def collectValidation(stats: Seq[Stat])(implicit c: AppContext): Seq[TermValidation] = {
    val validations = stats zip stats.map(_.pos) collect {
      case (q"validation[$tpe]($param){$func}", pos) =>
        val argName = param match {
          case q"$_.$n" => n
          case n => n
        }

        TermValidation(func, argName.syntax, pos)
    }

    validations.map(AppRegister.registerValidation)
  }
}
