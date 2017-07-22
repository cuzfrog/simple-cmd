package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._

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