package com.github.cuzfrog.scmd.macros
import scala.collection.immutable
import scala.meta.{Mod, Stat, Term, Type}

private[scmd] class ScmdRouteMacro extends ScmdMacro{
  override def expand(mods: immutable.Seq[Mod],
                      name: Type.Name,
                      ctorMods: immutable.Seq[Mod],
                      paramss: immutable.Seq[immutable.Seq[Term.Param]],
                      stats: immutable.Seq[Stat]): Stat = ???
}
