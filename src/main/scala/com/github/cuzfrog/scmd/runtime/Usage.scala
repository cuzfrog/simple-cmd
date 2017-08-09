package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.AppInfo
import com.github.cuzfrog.scmd.internal.AnsiFormatter.FormattedHelper

sealed trait ConsoleType
private[runtime] object ConsoleType {
  def detect: ConsoleType = new ConsoleType {} //todo: detect console type.
}

private[runtime] trait UsageEvidence[A] {
  def genUsage(a: A)(implicit consoleType: ConsoleType): String
}
private[runtime] trait ManualEvidence[A] {
  def genManual(a: A)(implicit consoleType: ConsoleType): String
}

private object UsageEvidence extends UsageUtils {
  implicit val defaultUsage: UsageEvidence[ArgTree] = new UsageEvidence[ArgTree] {
    override def genUsage(a: ArgTree)(implicit consoleType: ConsoleType): String = {
      implicit val builder: StringBuilder = StringBuilder.newBuilder
      ansi"%underline{Usage:}".line
      ansi"  %bold{${a.appName}} ".add()
      ansi"%yellow{parameters}"
        .condition(a.topParams.nonEmpty)
        .add(!a.topParams.exists(_.entity.isMandatory)).space
      ansi"%yellow{options}"
        .condition(a.topOpts.nonEmpty)
        .add(!a.topOpts.exists(_.entity.isMandatory)).space
      ansi"<command>".condition(a.cmdEntry.children.nonEmpty).add().space
      val subParamss = a.cmdEntry.children.flatMap(_.params)
      ansi"%yellow{parameters}"
        .condition(subParamss.nonEmpty)
        .add(!subParamss.exists(_.entity.isMandatory)).space
      val subOpts = a.cmdEntry.children.flatMap(_.opts)
      ansi"%yellow{options}"
        .condition(subOpts.nonEmpty).add(!subOpts.exists(_.entity.isMandatory)).space
      val subCmds = a.cmdEntry.children.flatMap(_.subCmdEntry.children)
      ansi" <sub-command> etc...".condition(subCmds.nonEmpty).line

      builder.mkString
    }
  }

  implicit val defaultCommandUsage: UsageEvidence[CmdNode] = new UsageEvidence[CmdNode] {
    override def genUsage(a: CmdNode)(implicit consoleType: ConsoleType): String = {
      ansi"%bold{${a.entity.name}} - ${a.entity.description}."
    }
  }
}

private object ManualEvidence extends UsageUtils {
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

private sealed trait UsageUtils {
  protected implicit class ExStringOps[S](in: S)
                                         (implicit builder: StringBuilder,
                                          conversion: S => BuilderHelper) {
    @inline def add(bracket: Boolean = false): BuilderHelper = {
      if (in.condition) {
        if (bracket) builder.append('[')
        builder.append(in.v)
        if (bracket) builder.append(']')
      }
      in
    }
    @inline def line: BuilderHelper = {
      if (in.condition) {
        builder.append(in.v).append(NEWLINE)
      }
      in
    }
    @inline def space: BuilderHelper = {
      if (in.condition) {
        builder.append(' ')
      }
      in
    }
    @inline def condition(on: Boolean): BuilderHelper = {
      in.condition = on
      in
    }
  }

  final class BuilderHelper(val v: String) {
    var condition: Boolean = true
  }
  object BuilderHelper {
    implicit def fromString(str: String): BuilderHelper = new BuilderHelper(str)
  }
}