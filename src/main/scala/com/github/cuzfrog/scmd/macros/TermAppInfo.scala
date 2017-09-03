package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.internal.RawArgMacro

import scala.collection.immutable
import scala.meta._

private case class TermAppInfo(name: String,
                               shortDescription: Option[Term] = None,
                               fullDescription: Option[Term] = None,
                               version: Option[Term] = None,
                               license: Option[Term] = None,
                               author: Option[Term] = None,
                               customs: immutable.Seq[(Term, Term)] = Nil)

private object TermAppInfo {

  private val NameExtractor = """(\w+)Defs?""".r
  def collectAppInfo(stats: immutable.Seq[Stat], name: Type.Name): TermAppInfo = {

    val inferredName = name.value match {
      case NameExtractor(n) => n.toLowerCase
      case n => n.toLowerCase
    }

    val basic = {
      val basicSeq = stats zip stats.map(_.pos) collect {
        case (q"appDef(..$params)", pos) =>
          implicit val position: Position = pos
          import RawArgMacro.extract
          val name = extract[String](params).getOrElse(inferredName)
          val shortDescription = extract[Term](params)
          val fullDescription = extract[Term](params)
          val version = extract[Term](params)
          val license = extract[Term](params)
          val author = extract[Term](params)
          TermAppInfo(name, shortDescription, fullDescription, version, license, author, Nil)
      }
      if (basicSeq.size > 1) abort(stats.last.pos, "appDef cannot be duplicated.")
      basicSeq.headOption.getOrElse(TermAppInfo(inferredName))
    }

    val customs: immutable.Seq[(Term, Term)] = {
      val customSeq = stats.collect {
        case q"appDefCustom(..$params)" =>
          params.map {
            case q"$n -> ${v:Term}" => n -> v
            case Term.Tuple(List(n, v: Term)) => n -> v
            case Term.Tuple(List(n, v)) =>
              abort(n.pos, s"Complex syntax '${v.syntax}' is not supported, in ${n.syntax}")
          }
      }
      if (customSeq.size > 1) abort(stats.last.pos, "appDefCustom cannot be duplicated.")
      customSeq.flatten
    }

    basic.copy(customs = customs)
  }

  implicit val definable: Definable[TermAppInfo] =
    new Definable[TermAppInfo] {
      override def defnTerm(a: TermAppInfo): Term = {
        val version = a.version match {
          case None => q"Option(getClass.getPackage.getImplementationVersion)"
          case vOpt => vOpt.defnTerm
        }

        val customTerm = a.customs.map { case (n, v) => Term.Tuple(List(n, v)) }
        q"""runtime.addAppInfo(
          name = ${a.name.defnTerm},
          shortDescription = ${a.shortDescription.defnTerm},
          fullDescription = ${a.fullDescription.defnTerm},
          version = $version,
          license = ${a.license.defnTerm},
          author = ${a.author.defnTerm},
          custom = Seq(..$customTerm)
         )"""
      }
    }
}
