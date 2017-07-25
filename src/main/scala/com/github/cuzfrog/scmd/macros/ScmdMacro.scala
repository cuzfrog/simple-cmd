package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.ScmdDef

import scala.collection.immutable
import scala.meta._
import scala.reflect.ClassTag

/**
  * Macro implementation interface.
  */
private[scmd] trait ScmdMacro {
  def expand(mods: immutable.Seq[Mod],
             name: Type.Name,
             ctorMods: immutable.Seq[Mod],
             paramss: immutable.Seq[immutable.Seq[Term.Param]],
             stats: immutable.Seq[Stat]): Stat
}

private[scmd] object MacroUtil {
  def apply(macroImpl: ScmdMacro, defn: Tree): Stat = {
    defn match {
      case q"..$mods class $name ..$ctorMods (...$paramss) { ..$stats }" =>
        macroImpl.expand(mods, name, ctorMods, paramss, stats)
      case _ =>
        abort(s"@${macroImpl.getClass.getSimpleName} must annotate a class.")
    }
  }
}