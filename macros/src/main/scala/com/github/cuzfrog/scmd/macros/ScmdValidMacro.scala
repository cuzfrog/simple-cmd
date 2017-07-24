package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._


private[scmd] class ScmdValidMacro extends ScmdMacro {
  final def expand(mods: immutable.Seq[Mod],
                   name: Type.Name,
                   ctorMods: immutable.Seq[Mod],
                   paramss: immutable.Seq[immutable.Seq[Term.Param]],
                   stats: immutable.Seq[Stat]): Stat = {

    val defClassName = paramss.flatten.headOption match {
      case Some(param"..$mods $name:$tpe") => name
      case _ => abort("ValidationClass should have DefClass as its first argument.")
    }

    /**
      * Client defined validations against arguments.
      *
      * This step extracts validation from class stats.
      */
    val validations = TermValidation.collectValidation(stats)

    val addStats = validations.map { tv =>
      q"${Term.Name(defClassName.value)}.addValidation(${Lit.String(tv.argName)},${tv.func})"
    }

    q"""..$mods class $name ..$ctorMods (...$paramss){
          ..$addStats
        }"""
  }
}
