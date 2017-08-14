package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.macros.MacroUtil

import scala.annotation.StaticAnnotation
import scala.meta._

final class ScmdDefTest extends StaticAnnotation{
  inline def apply(defn: Any): Any = meta {
    MacroUtil('DefTest, defn)
  }
}