package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.internal.AppInfoMacro

import scala.collection.immutable

@AppInfoMacro
private case class AppInfo(name: Option[String] = None,
                           shortDescription: Option[String] = None,
                           fullDescription: Option[String] = None,
                           version: Option[String] = None,
                           license: Option[String] = None,
                           author: Option[String] = None,
                           custom: immutable.Seq[(String, String)] = immutable.Seq.empty)

private object AppInfo {
  /**
    * Order of namespaces is kept. Order is controlled by custom names.
    * Namespace in custom has priority. Value in basic names has priority.
    */
  private[scmd] implicit val canFormPrettyString: CanFormPrettyString[AppInfo] = (a: AppInfo) => {
    val basics: Seq[(String, String)] = a.combineBasics
    val filledCustom = a.custom.map {
      case (n, v) => basics.collectFirst { case (bn, bv) if bn == n => bv } match {
        case Some(bv) => n -> bv //fill custom with basic
        case None => n -> v
      }
    }

    val top = basics.collect { case (n, v) if !filledCustom.exists(_._1 == n) => (n, v) }

    (top ++ filledCustom).mkString(System.lineSeparator)
  }
}