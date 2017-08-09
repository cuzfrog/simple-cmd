package com.github.cuzfrog.scmd.runtime.console

import com.github.cuzfrog.scmd.AppInfo
import com.github.cuzfrog.scmd.ScmdUtils._
import com.github.cuzfrog.scmd.runtime.{ArgTree, CmdNode, OptNode, ParamNode, PriorNode, PropNode}

private sealed trait UsageNode

private case class UsageArgTree(appInfo: AppInfo,
                                topParams: Seq[UsageParamNode],
                                topOpts: Seq[UsageOptNode],
                                priors: Seq[UsagePriorNode],
                                props: Seq[UsagePropNode])

private case class UsageCmdNode(name: String,
                                description: String,
                                params: Seq[UsageParamNode],
                                opts: Seq[UsageOptNode],
                                subCmds: Seq[UsageCmdNode]) extends UsageNode

private case class UsageParamNode(name: String,
                                  description: String, descrOffset: Int = 0) extends UsageNode
private case class UsageOptNode(abbr: String, name: String,
                                description: String, descrOffset: Int = 0) extends UsageNode
private case class UsagePriorNode(name: String, alias: Seq[String],
                                  description: String, descrOffset: Int = 0) extends UsageNode
private case class UsagePropNode(name: String, flag: String,
                                 description: String, descrOffset: Int = 0) extends UsageNode

private object UsageArgTree {
  implicit val argTree2usageEv: Convertible[ArgTree, UsageArgTree] = (a: ArgTree) => {
    val topParams = a.topParams.map(_.convertTo[UsageParamNode])
    val topOpts = a.topOpts.map(_.convertTo[UsageOptNode])
  }

  private def recConvertCmdNode(a: CmdNode): UsageCmdNode = {
    val name = s"<${a.entity.originalName}>"
    val description = a.entity.description.map(dscr => s": $dscr").getOrElse("")
    val params = a.params.map(_.convertTo[UsageParamNode])
    val opts = a.opts.map(_.convertTo[UsageOptNode])
    val subCmds = a.subCmdEntry.children.map(recConvertCmdNode)
    UsageCmdNode(name, description, params, opts, subCmds)
  }

  def align(seq: Seq[UsageNode]): Seq[UsageNode] = {
    ???
  }

  implicit def paramNode2usageEv[T]: Convertible[ParamNode[T], UsageParamNode] =
    (a: ParamNode[T]) => {
      val name = s"<${a.entity.originalName}>"
      val description = a.entity.description.map(dscr => s": $dscr").getOrElse("")
      UsageParamNode(name, description)
    }

  implicit def optNode2usageEv[T]: Convertible[OptNode[T], UsageOptNode] =
    (a: OptNode[T]) => {
      val abbr = a.entity.originalAbbr.map(ab => s"$ab ").getOrElse("")
      val name = s"<${a.entity.originalName}>"
      val description = a.entity.description.map(dscr => s": $dscr").getOrElse("")
      UsageOptNode(abbr, name, description)
    }

  implicit val priorNode2usageEv: Convertible[PriorNode, UsagePriorNode] =
    (a: PriorNode) => {
      val name = a.entity.originalName
      val alias = a.entity.alias
      val description = a.entity.description.map(dscr => s": $dscr").getOrElse("")
      UsagePriorNode(name, alias, description)
    }

  implicit def propNode2usageEv[T]: Convertible[PropNode[T], UsagePropNode] =
    (a: PropNode[T]) => {
      val name = a.entity.originalName
      val flag = a.entity.flag
      val description = a.entity.description.map(dscr => s": $dscr").getOrElse("")
      UsagePropNode(name, flag, description)
    }
}