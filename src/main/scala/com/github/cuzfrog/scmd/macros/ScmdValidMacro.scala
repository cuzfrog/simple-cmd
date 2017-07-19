package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta.{Defn, Stat, Type}

/**
  * Created by cuz on 7/19/17.
  */
private[scmd] class ScmdValidMacro extends ScmdMacro {
  override def expand(name: Type.Name, stats: immutable.Seq[Stat]): Defn.Class = ???
}
