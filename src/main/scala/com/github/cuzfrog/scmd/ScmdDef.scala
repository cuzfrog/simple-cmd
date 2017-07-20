package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.macros.{ScmdDefMacro, ScmdMacro}

import scala.annotation.StaticAnnotation
import scala.meta._

//MacroUtil has to be in the same file.
private object MacroUtil {
  def apply(macroImpl: ScmdMacro, defn: Tree, classDefs: RuntimeClassDefs.type): Stat = {
    defn match {
      case q"..$mods class $name { ..$stats }" => macroImpl.expand(name, stats, classDefs)
      case _ =>
        abort(s"@${macroImpl.getClass.getSimpleName} must annotate a class.")
    }
  }
}

final class ScmdDef extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    MacroUtil(new ScmdDefMacro, defn, RuntimeClassDefs)
  }
}

//final class ScmdValid extends StaticAnnotation {
//  inline def apply(defn: Any): Any = meta {
//    MacroUtil(new ScmdValidMacro, defn)
//  }
//}

private object RuntimeClassDefs {

}