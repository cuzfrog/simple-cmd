package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.macros.{ScmdDefMacro, ScmdMacro, ScmdValidMacro}

import scala.annotation.StaticAnnotation
import scala.meta._

//MacroUtil has to be in the same file.
private object MacroUtil {
  println(this.toString)
  def apply(macroImpl: ScmdMacro, defn: Tree): Stat = {
    defn match {
      case q"..$mods class $name ..$ctorMods (...$paramss) { ..$stats }" =>
        macroImpl.expand(mods, name, ctorMods, paramss, stats)
      case _ =>
        abort(s"@${macroImpl.getClass.getSimpleName} must annotate a class.")
    }
  }
}

final class ScmdDef extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    MacroUtil(new ScmdDefMacro, defn)
  }
}

final class ScmdValid extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    MacroUtil(new ScmdValidMacro, defn)
  }
}
