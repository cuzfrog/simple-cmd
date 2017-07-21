package com.github.cuzfrog.scmd.internal

import scala.annotation.StaticAnnotation
import scala.meta._
import scala.collection.immutable

private[scmd] class AppInfoMacro extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(
      Seq(q"..$mods class $tname ..$ctorMods (...$paramss)", companion: Defn.Object)) =>
        val basics = paramss.head.collect {
          case param"..$mods $name: Option[String] = $_" =>
            q"${Term.Name(name.value)}.map(v=>(${Lit.String(name.value)},v))"
        }

        val combineDef =
          q"""private def combineBasics:Seq[(String,String)] = Seq(
                ..$basics
              ).flatten"""

        val result = Term.Block(immutable.Seq(
          q"..$mods class $tname ..$ctorMods (...$paramss){$combineDef}", companion))
        //println(result.syntax)
        result
      case _ =>
        abort("This only work on AppInfo.")
    }
  }
}
