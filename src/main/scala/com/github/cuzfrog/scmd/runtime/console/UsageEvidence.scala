package com.github.cuzfrog.scmd.runtime.console

import com.github.cuzfrog.scmd.runtime.{ArgTree, CmdNode, ParamNode, PriorNode}
import com.github.cuzfrog.scmd.internal.AnsiFormatter.FormattedHelper

private[runtime] trait UsageEvidence[A] {
  def genUsage(a: A)(implicit consoleType: ConsoleType,
                     builder: StringBuilder): String
}


private object UsageEvidence extends BuilderUtils {
  implicit val defaultUsage: UsageEvidence[ArgTree] = new UsageEvidence[ArgTree] {
    override def genUsage(a: ArgTree)(implicit consoleType: ConsoleType,
                                      builder: StringBuilder): String = {
      ansi"%underline{Usage:}".line
      ansi"  %bold{${a.appName}} ".add()
      ansi"%yellow{parameters}"
        .condition(a.topParams.nonEmpty)
        .add(!a.topParams.exists(_.entity.isMandatory)).space
      ansi"%yellow{options}"
        .condition(a.topOpts.nonEmpty)
        .add(!a.topOpts.exists(_.entity.isMandatory)).space
      ansi"[%yellow{properties}]"
        .condition(a.props.nonEmpty).add().space
      ansi"<command>".condition(a.cmdEntry.children.nonEmpty).add().space
      val subParamss = a.cmdEntry.children.flatMap(_.params)
      ansi"%yellow{parameters}"
        .condition(subParamss.nonEmpty)
        .add(!subParamss.exists(_.entity.isMandatory)).space
      val subOpts = a.cmdEntry.children.flatMap(_.opts)
      ansi"%yellow{options}"
        .condition(subOpts.nonEmpty).add(!subOpts.exists(_.entity.isMandatory)).space
      val subCmds = a.cmdEntry.children.flatMap(_.subCmdEntry.children)
      ansi"<sub-command> ...".condition(subCmds.nonEmpty).line

      a.toTopNode

      //a.priors
      builder.mkString
    }

    private def recGenUsage(a: CmdNode, indent: Int = 1)
                           (implicit builder: StringBuilder): Unit = {
      ansi"${a.entity.name} - ${a.entity.description}".line

    }
  }

  implicit val defaultCommandUsage: UsageEvidence[CmdNode] = new UsageEvidence[CmdNode] {
    override def genUsage(a: CmdNode)(implicit consoleType: ConsoleType,
                                      builder: StringBuilder): String = {
      ansi"%bold{${a.entity.name}} - ${a.entity.description}."
    }
  }

  implicit val defaultParamUsage: UsageEvidence[ParamNode[_]] = new UsageEvidence[ParamNode[_]] {
    override def genUsage(a: ParamNode[_])(implicit consoleType: ConsoleType,
                                           builder: StringBuilder): String = {

      ansi"%yellow{${a.entity.name}} - ${a.entity.description}."
    }
  }

  implicit val defaultPriorUsage: UsageEvidence[PriorNode] = new UsageEvidence[PriorNode] {
    override def genUsage(a: PriorNode)(implicit consoleType: ConsoleType,
                                        builder: StringBuilder): String = {
      ansi"%bold{${a.entity.name}} - ${a.entity.description}."
    }
  }
}
