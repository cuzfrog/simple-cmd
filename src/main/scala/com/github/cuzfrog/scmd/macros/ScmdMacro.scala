package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._

/**
  * Macro implementation interface.
  */
private[scmd] trait ScmdMacro {
  def expand(name: Type.Name,
             paramss: immutable.Seq[immutable.Seq[Term.Param]],
             stats: immutable.Seq[Stat]): Stat
}