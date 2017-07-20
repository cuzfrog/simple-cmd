package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.internal.RawArgMacro
import com.github.cuzfrog.scmd.macros.Constants._

import scala.meta._

private case class TermAppInfo(term: Term)

private object TermAppInfo {

  def collectAppInfo(stats: Seq[Stat]): TermAppInfo = {
    stats zip stats.map(_.pos) collect {
      case (q"appDef(..$params)", pos) =>
        implicit val position = pos
        val shortDescription = RawArgMacro.extract[String](params)
        println(shortDescription)
    }

    stats.collect {
      case q"appDefCustom(..$params)" =>
    }

    TermAppInfo(q"scmdRuntime.addAppInfo(name = ${Lit.String("dummy-app")})")
  }
}

////    require(Option(appName).nonEmpty, "appName cannot be null.") //guards
////    require(appName != "", "appName cannot be empty. You could leave as default, which is: App")
