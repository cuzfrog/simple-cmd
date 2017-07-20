package com.github.cuzfrog.scmd.macros

import scala.meta._

private case class TermAppInfo(term: Term)

private object TermAppInfo {
  def collectAppInfo(stats: Seq[Stat]): TermAppInfo = {
    stats.collect {
      case q"appDef(..$params)" =>
    }

    stats.collect{
      case q"appDefCustom(..$params)" =>
    }

    TermAppInfo(q"scmdRuntime.addAppInfo(name = ${Lit.String("dummy-app")})")
  }
}