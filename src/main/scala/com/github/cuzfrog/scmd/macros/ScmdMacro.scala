package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.RuntimeClassDefs

import scala.collection.immutable
import scala.meta._

/**
  * Macro implementation interface.
  */
private[scmd] trait ScmdMacro {
  def expand(name: Type.Name, stats: immutable.Seq[Stat], classDefs: RuntimeClassDefs.type): Stat
}