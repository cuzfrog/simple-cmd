package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.AppInfo
import com.github.cuzfrog.scmd.internal.RawArgMacro
import com.github.cuzfrog.scmd.macros.Constants._

import scala.meta._

private case class TermAppInfo(term: Term)

private object TermAppInfo {

  def collectAppInfo(stats: Seq[Stat]): TermAppInfo = {

    val basicSeq = stats zip stats.map(_.pos) collect {
      case (q"appDef(..$params)", pos) =>
        implicit val position = pos
        import RawArgMacro.extract
        val name = extract[String](params).getOrElse("App") //todo: app name
      val shortDescription = extract[String](params)
        val fullDescription = extract[String](params)
        val version = extract[String](params)
        val license = extract[String](params)
        val author = extract[String](params)
        AppInfo(name, shortDescription, fullDescription, version, license, author)
    }

    val customSeq: Seq[AppInfo] = stats.collect {
      case q"appDefCustom(..$params)" => ???
    }

    if (basicSeq.size > 1 || customSeq.size > 1) abort(stats.last.pos, "appDef cannot be duplicated.")

    val appInfo = (basicSeq.headOption, customSeq.headOption) match {
      case (Some(basic), Some(custom)) => basic.copy(custom = custom.custom)
      case (Some(basic), None) => basic
      case (None, Some(custom)) => custom
      case (None, None) => AppInfo("App")
    }

    TermAppInfo(appInfo.defnTerm)
  }


  implicit val definable: Definable[AppInfo] = new Definable[AppInfo] {
    override def defnTerm(a: AppInfo): Term = {
      ???
      q"scmdRuntime.addAppInfo(name = ${Lit.String("dummy-app")})"
    }
  }
}

////    require(Option(appName).nonEmpty, "appName cannot be null.") //guards
////    require(appName != "", "appName cannot be empty. You could leave as default, which is: App")
