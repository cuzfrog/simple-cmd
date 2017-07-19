package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._

/**
  * Macro implementation interface.
  */
private[scmd] trait ScmdMacro {
  def expand(name: Type.Name, stats: immutable.Seq[Stat]): Defn.Class
}