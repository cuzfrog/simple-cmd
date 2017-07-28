package com.github.cuzfrog.scmd.internal

import scala.annotation.StaticAnnotation
import scala.meta._

private[scmd] class IgnoreLogging extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods def $ename[..$tparams](...$paramss): $tpeopt = $expr"
        if {
          val modsStr = mods.map(_.structure)
          modsStr.contains(mod"abstract".structure) && modsStr.contains(mod"override".structure)
        } =>
        val paramssTerm = paramss.map(_.map(param => Term.Name(param.name.value)))
        val tparamsTerm = tparams.map(tparam => Type.Name(tparam.name.value))
        if (paramssTerm.nonEmpty && tparamsTerm.nonEmpty) {
          q"""..$mods def $ename[..$tparams](...$paramss): $tpeopt =
           super.$ename[..$tparamsTerm](...$paramssTerm)
          """
        } else if (paramssTerm.nonEmpty && tparamsTerm.isEmpty) {
          q"""..$mods def $ename(...$paramss): $tpeopt =
           super.$ename(...$paramssTerm)
          """
        } else if (paramssTerm.isEmpty && tparamsTerm.nonEmpty) {
          q"""..$mods def $ename[..$tparams]: $tpeopt =
           super.$ename[..$tparamsTerm]
          """
        } else {
          q"""..$mods def $ename: $tpeopt =
           super.$ename
          """
        }
      case _ =>
        abort("@IgnoreLogging can only be used on override def with super callable.")
    }
  }
}
