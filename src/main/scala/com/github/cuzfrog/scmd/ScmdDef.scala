package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.macros.{MacroUtil, ScmdDefMacro, ScmdMacro, ScmdValidMacro}

import scala.annotation.StaticAnnotation
import scala.meta._



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
