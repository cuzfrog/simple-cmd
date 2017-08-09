package com.github.cuzfrog.scmd.runtime.console

import com.github.cuzfrog.scmd.runtime.{ArgTree, CmdNode, OptNode, ParamNode, PriorNode}
import com.github.cuzfrog.scmd.internal.AnsiFormatter.FormattedHelper

private[runtime] trait UsageEvidence[A] {
  def genUsage(a: A)
              (implicit consoleType: ConsoleType,
               builder: StringBuilder, indent: Indent): StringBuilder
}


private object UsageEvidence extends BuilderUtils {
  implicit val defaultUsage: UsageEvidence[ArgTree] = new UsageEvidence[ArgTree] {
    override def genUsage(a: ArgTree)
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      ansi"%underline{Usage:}".add.line
      ansi"  %bold{${a.appName}} ".add
      ansi"%yellow{parameters}"
        .condition(a.topParams.nonEmpty)
        .add(!a.topParams.exists(_.entity.isMandatory)).space
      ansi"%yellow{options}"
        .condition(a.topOpts.nonEmpty)
        .add(!a.topOpts.exists(_.entity.isMandatory)).space
      ansi"[%yellow{properties}]"
        .condition(a.props.nonEmpty).add.space
      ansi"<command>".condition(a.cmdEntry.children.nonEmpty).add.space
      val subParamss = a.cmdEntry.children.flatMap(_.params)
      ansi"%yellow{parameters}"
        .condition(subParamss.nonEmpty)
        .add(!subParamss.exists(_.entity.isMandatory)).space
      val subOpts = a.cmdEntry.children.flatMap(_.opts)
      ansi"%yellow{options}"
        .condition(subOpts.nonEmpty).add(!subOpts.exists(_.entity.isMandatory)).space
      val subCmds = a.cmdEntry.children.flatMap(_.subCmdEntry.children)
      ansi"<sub-command> ...".condition(subCmds.nonEmpty).add
      newline
      ansi"%underline{Description:}".add.line
      a.toTopNode.genUsage
      //a.priors
      builder
    }
  }

  implicit val defaultCommandUsage: UsageEvidence[CmdNode] = new UsageEvidence[CmdNode] {
    override def genUsage(a: CmdNode)
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      recGenUsage(a, indent)
      builder
    }

    private def recGenUsage(a: CmdNode, indent: Indent)
                           (implicit builder: StringBuilder): Unit = {
      implicit val in: Indent = indent
      ansi"%bold{${a.entity.name}} : ${a.entity.description}".indent(indent).add.line
      a.params.foreach(_.genUsage)
      a.opts.foreach(_.genUsage)
      a.subCmdEntry.children.foreach { e => recGenUsage(e, indent + 2) }
    }
  }

  implicit def defaultParamUsage[T]: UsageEvidence[ParamNode[T]] = new UsageEvidence[ParamNode[T]] {
    override def genUsage(a: ParamNode[T])
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      ansi"%yellow{${a.entity.originalName}}".indent(indent + 1).add(!a.entity.isMandatory)
      a.entity.description.foreach(dscr => s" : $dscr".add)
      newline
      builder
    }
  }

  implicit def defaultOptUsage[T]: UsageEvidence[OptNode[T]] = new UsageEvidence[OptNode[T]] {
    override def genUsage(a: OptNode[T])
                         (implicit consoleType: ConsoleType,
                          builder: StringBuilder, indent: Indent): StringBuilder = {
      val abbr = a.entity.originalAbbr.map(ab => ansi"|%yellow{$ab}").getOrElse("")
      ansi"%yellow{${a.entity.originalName}}$abbr".indent(indent + 1).add(!a.entity.isMandatory)
      a.entity.description.foreach(dscr => s" : $dscr".add)
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
