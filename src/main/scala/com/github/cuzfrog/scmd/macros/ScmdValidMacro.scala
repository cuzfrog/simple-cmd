package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta.{Defn, Stat, Type}


private[scmd] class ScmdValidMacro extends ScmdMacro {
  final def expand(name: Type.Name, stats: immutable.Seq[Stat]): Stat = {
    ???
  }
}
