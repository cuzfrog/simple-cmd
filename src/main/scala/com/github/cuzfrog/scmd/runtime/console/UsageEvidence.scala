package com.github.cuzfrog.scmd.runtime.console

import com.github.cuzfrog.scmd.runtime.{ArgTree, CmdNode, OptNode, ParamNode, PriorNode}
import com.github.cuzfrog.scmd.internal.AnsiFormatter.FormattedHelper

private[runtime] trait UsageEvidence[A] {
  def genUsage(a: A)
              (implicit consoleType: ConsoleType,
               builder: StringBuilder, indent: Indent): StringBuilder
}


private object UsageEvidence extends BuilderUtils {
  implicit val defaultUsage: UsageEvidence[UsageArgTree] = new UsageEvidence[UsageArgTree] {
    override def genUsage(a: UsageArgTree)
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      // -------- Section1 One line usage -------
      ansi"%underline{Usage:}".add.line
      ansi"  %bold{${a.appInfo.name}} ".add
      ansi"%yellow{parameters}"
        .condition(a.topParams.nonEmpty)
        .add(!a.topParams.exists(_.isMandatory)).space
      ansi"%yellow{options}"
        .condition(a.topOpts.nonEmpty)
        .add(!a.topOpts.exists(_.isMandatory)).space
      ansi"[%yellow{properties}]"
        .condition(a.props.nonEmpty).add.space
      ansi"<command>".condition(a.subCmds.nonEmpty).add.space
      val subParamss = a.subCmds.flatMap(_.params)
      ansi"%yellow{<parameters>}"
        .condition(subParamss.nonEmpty)
        .add(!subParamss.exists(_.isMandatory)).space
      val subOpts = a.subCmds.flatMap(_.opts)
      ansi"%yellow{options}"
        .condition(subOpts.nonEmpty).add(!subOpts.exists(_.isMandatory)).space
      val subCmds = a.subCmds.flatMap(_.subCmds)
      ansi"<sub-command> ...".condition(subCmds.nonEmpty).add
      newline
      // -------- Section2 properties -------
      ansi"%underline{Properties:}".condition(a.props.nonEmpty).add.line
      a.props.foreach(_.genUsage)
      // -------- Section3 arguments info -------
      ansi"%underline{Descr:}".add.line
      a.toTopNode.genUsage

      builder
    }
  }

  implicit val defaultCommandUsage: UsageEvidence[UsageCmdNode] = new UsageEvidence[UsageCmdNode] {
    override def genUsage(a: UsageCmdNode)
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      recGenUsage(a, indent)
      builder
    }

    private def recGenUsage(a: UsageCmdNode, indent: Indent)
                           (implicit builder: StringBuilder): Unit = {
      implicit val in: Indent = indent
      ansi"%bold{${a.name}} ".indent(indent).add
      a.params.foreach(p=> ansi"%yellow{${p.name}} ".add)
      a.description.add
      newline

      UsageArgTree.alignOpts(a.opts).foreach(_.genUsage)
      a.subCmds.foreach { e => recGenUsage(e, indent + 2) }
    }
  }

  implicit val defaultParamUsage: UsageEvidence[UsageParamNode] = new UsageEvidence[UsageParamNode] {
    override def genUsage(a: UsageParamNode)
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      ansi"%yellow{${a.name}}".indent(indent + 1).add
      a.description.indent(a.descrOffset).add
      newline
      builder
    }
  }

  implicit val defaultOptUsage: UsageEvidence[UsageOptNode] = new UsageEvidence[UsageOptNode] {
    override def genUsage(a: UsageOptNode)
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      val abbr = ansi"%yellow{${a.abbr}}"
      ansi"$abbr%yellow{${a.name}}".indent(indent + 1).add
      a.description.indent(a.descrOffset).add
      newline
      builder
    }
  }

  implicit val defaultPropUsage: UsageEvidence[UsagePropNode] = new UsageEvidence[UsagePropNode] {
    override def genUsage(a: UsagePropNode)
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      ansi"${a.flag}%dim{key=value}".indent(indent).add
      a.description.indent(a.descrOffset).add
      newline
      builder
    }
  }
  //  implicit val defaultPriorUsage: UsageEvidence[PriorNode] = new UsageEvidence[PriorNode] {
  //    override def genUsage(a: PriorNode)(implicit consoleType: ConsoleType,
  //                                        builder: StringBuilder,
  //                                        indent: Indent): StringBuilder = {
  //      ansi"%bold{${a.entity.name}} - ${a.entity.description}."
  //    }
  //  }
}
