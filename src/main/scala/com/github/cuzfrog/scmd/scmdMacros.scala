package com.github.cuzfrog.scmd

import scala.collection.immutable
import scala.meta._

class ScmdDef extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"object $name { ..$stats }" =>
        ScmdDefMacroImpl.expand(name, stats)
      case _ =>
        abort("@main must annotate an object.")
    }
  }
}

private object ScmdDefMacroImpl {
  def expand(name: Term.Name, stats: immutable.Seq[Stat]): Defn.Object = {
    val parseMethod =
      q"""
         /** Parse arguments. */
         def parse(args: Array[String]): Unit = { args.foreach(println) }
        """
    val moreStats = stats :+ parseMethod

    q"object $name { ..$moreStats }"
  }
}

class main extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"object $name { ..$stats }" = defn
    val main = q"def main(args: Array[String]): Unit = { ..$stats }"
    q"object $name { $main }"
  }
}