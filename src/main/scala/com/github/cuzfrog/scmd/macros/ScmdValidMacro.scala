package com.github.cuzfrog.scmd.macros

import scala.meta._

private class ScmdValidMacro extends ScmdMacro {
  def expand(cls: Defn.Class): Stat = {
    val mods = cls.mods
    val name = cls.name
    val ctorMods = cls.ctor.mods
    val paramss = cls.ctor.paramss
    val stats = cls.templ.stats.getOrElse(Nil)

    val defClassName = paramss.flatten.headOption match {
      case Some(param"..$_ $name:$_") => name
      case _ => abort("ValidationClass should have DefClass as its first argument.")
    }

    val imports = stats.collect { case i: Import => i}

    /**
      * Client defined validations against arguments.
      *
      * This step extracts validation from class stats.
      */
    val validations = TermValidation.collectValidation(stats)

    val addStats = validations.map { tv =>
      q"${Term.Name(defClassName.value)}.addValidation(${tv.argName},${tv.term})"
    }

    q"""..$mods class $name ..$ctorMods (...$paramss){
          ..$imports
          ..$addStats
        }"""
  }
}
