package com.github.cuzfrog.scmd.runtime.console

import com.github.cuzfrog.scmd.AppInfo
import com.github.cuzfrog.scmd.runtime.{ArgTree, CmdNode}
import com.github.cuzfrog.scmd.internal.AnsiFormatter.FormattedHelper

private[runtime] trait ManualEvidence[A] {
  def genManual(a: A)(implicit consoleType: ConsoleType): String
}


private object ManualEvidence extends BuilderUtils {
  implicit val defaultManual: ManualEvidence[(AppInfo, ArgTree)] =
    new ManualEvidence[(AppInfo, ArgTree)] {
      override def genManual(a: (AppInfo, ArgTree))(implicit consoleType: ConsoleType): String = {
        implicit val builder: StringBuilder = StringBuilder.newBuilder
        val (appInfo, argTree) = a
        ansi"%underline{Usage:}".line
        ansi"%bold{Usage:}"
        builder.mkString
      }
    }

  implicit val defaultCmdNodeManual: ManualEvidence[CmdNode] =
    new ManualEvidence[CmdNode] {
      override def genManual(a: CmdNode)(implicit consoleType: ConsoleType): String = {
        ansi"%bold{${a.entity.name}} - ${a.entity.description}."
      }
    }
}

//todo: implementation