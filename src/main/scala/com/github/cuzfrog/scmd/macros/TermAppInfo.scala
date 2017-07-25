package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.AppInfo
import com.github.cuzfrog.scmd.internal.RawArgMacro

import scala.meta._

private case class TermAppInfo(term: Term, appInfo: AppInfo)

private object TermAppInfo {

  def collectAppInfo(stats: Seq[Stat]): TermAppInfo = {

    val basicSeq = stats zip stats.map(_.pos) collect {
      case (q"appDef(..$params)", pos) =>
        implicit val position = pos
        import RawArgMacro.extract
        val name = extract[String](params)
        val shortDescription = extract[String](params)
        val fullDescription = extract[String](params)
        val version = extract[String](params)
        val license = extract[String](params)
        val author = extract[String](params)
        AppInfo(name, shortDescription, fullDescription, version, license, author)
    }

    val customSeq: Seq[AppInfo] = stats.collect {
      case q"appDefCustom(..$params)" =>
        val custom = params.collect {
          case q"${Lit.String(n)} -> ${Lit.String(v)}" if Option(v).nonEmpty => n -> v
        }
        AppInfo(custom = custom)
    }

    if (basicSeq.size > 1 || customSeq.size > 1) abort(stats.last.pos, "appDef cannot be duplicated.")

    val appInfo = (basicSeq.headOption, customSeq.headOption) match {
      case (Some(basic), Some(custom)) => basic.copy(custom = custom.custom)
      case (Some(basic), None) => basic
      case (None, Some(custom)) => custom
      case (None, None) => AppInfo()
    }
    TermAppInfo(appInfo.defnTerm, appInfo)
  }


  implicit val definable: Definable[AppInfo] = new Definable[AppInfo] {
    override def defnTerm(a: AppInfo): Term = {

      val customTerm = a.custom.map { case (n, v) => q"(${Lit.String(n)}, ${Lit.String(v)})" }
      q"""runtime.addAppInfo(
          name = ${a.name.toTerm},
          shortDescription = ${a.shortDescription.toTerm},
          fullDescription = ${a.fullDescription.toTerm},
          version = ${a.version.toTerm},
          license = ${a.license.toTerm},
          author = ${a.author.toTerm},
          custom = Seq(..$customTerm)
         )"""
    }

    private implicit class OptionTermOps(in: Option[String]) {
      def toTerm: Term = in match {
        case Some(s) => q"Option(${Lit.String(s)})"
        case None => q"None"
      }
    }
  }
}

////    require(Option(appName).nonEmpty, "appName cannot be null.") //guards
////    require(appName != "", "appName cannot be empty. You could leave as default, which is: App")
