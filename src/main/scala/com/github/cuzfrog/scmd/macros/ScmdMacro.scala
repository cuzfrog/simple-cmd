package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._

/**
  * Macro implementation interface.
  */
private trait ScmdMacro {
  def expand(mods: immutable.Seq[Mod],
             name: Type.Name,
             ctorMods: immutable.Seq[Mod],
             paramss: immutable.Seq[immutable.Seq[Term.Param]],
             stats: immutable.Seq[Stat]): Stat
}

object MacroUtil {
  def apply(implName: scala.Symbol, defn: Tree): Stat = {

    val macroImpl: ScmdMacro = implName match {
      case 'Def => new ScmdDefMacro
      case 'Valid => new ScmdValidMacro
      case bad => throw new AssertionError(s"No such ScmdMacro implementation:$bad")
    }

    defn match {
      case q"..$mods class $name ..$ctorMods (...$paramss) { ..$stats }" =>
        macroImpl.expand(mods, name, ctorMods, paramss, stats)
      case Term.Block(
      Seq(q"..$mods class $name ..$ctorMods (...$paramss) { ..$stats }",
      companion: Defn.Object)) =>
        Term.Block(
          immutable.Seq(macroImpl.expand(mods, name, ctorMods, paramss, stats), companion)
        )
      case _ =>
        abort(s"@${macroImpl.getClass.getSimpleName} must annotate a class.")
    }
  }
}