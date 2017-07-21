package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._


private[scmd] class ScmdValidMacro extends ScmdMacro {
  final def expand(name: Type.Name,
                   paramss: immutable.Seq[Term.Param],
                   stats: immutable.Seq[Stat]): Stat = {
    ???
  }
}
