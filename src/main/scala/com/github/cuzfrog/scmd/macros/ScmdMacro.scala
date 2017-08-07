package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._

/**
  * Macro implementation interface.
  */
private trait ScmdMacro {
  def expand(cls: Defn.Class): Stat
}

/** Entry for scmd macro, this has to be public for client Api package Scmd does not enclose this. */
object MacroUtil {
  def apply(implName: scala.Symbol, defn: Tree): Stat = {

    val macroImpl: ScmdMacro = implName match {
      case 'Def => new ScmdDefMacro
      case 'Valid => new ScmdValidMacro
      case bad => throw new AssertionError(s"No such ScmdMacro implementation:$bad")
    }
    //todo: mods cannot contain final
    defn match {
      case cls: Defn.Class => macroImpl.expand(cls)
      case Term.Block(Seq(cls: Defn.Class, companion: Defn.Object)) =>
        Term.Block(immutable.Seq(macroImpl.expand(cls), companion))
      case _ =>
        abort(s"@${macroImpl.getClass.getSimpleName} must annotate a class.")
    }
  }
}