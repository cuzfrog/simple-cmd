package com.github.cuzfrog.scmd.runtime.console

import com.github.cuzfrog.scmd.AppInfo
import com.github.cuzfrog.scmd.ScmdUtils._
import com.github.cuzfrog.scmd.runtime.{ArgTree, CmdNode, OptNode, ParamNode, PriorNode, PropNode}

private sealed trait UsageNode {
  def name: String
  def descrLHwidth: Int = name.length
}

private[runtime]
case class UsageArgTree(appInfo: AppInfo,
                        private[console] val topParams: Seq[UsageParamNode],
                        private[console] val topOpts: Seq[UsageOptNode],
                        private[console] val priors: Seq[UsagePriorNode],
                        private[console] val props: Seq[UsagePropNode],
                        private[console] val subCmds: Seq[UsageCmdNode]) {
  def toTopNode: UsageCmdNode = UsageCmdNode(
    appInfo.name,
    appInfo.shortDescription.map(s => s": $s").getOrElse(""),
    topParams,
    topOpts,
    subCmds
  )
}

private[console] case class UsageCmdNode(name: String,
                                         description: String,
                                         params: Seq[UsageParamNode],
                                         opts: Seq[UsageOptNode],
                                         subCmds: Seq[UsageCmdNode]) extends UsageNode

private[console] case class UsageParamNode(name: String, description: String,
                                           isMandatory: Boolean,
                                           descrOffset: Int = 0) extends UsageNode
private[console] case class UsageOptNode(abbr: String, name: String, description: String,
                                         isMandatory: Boolean,
                                         descrOffset: Int = 0) extends UsageNode {
  override def descrLHwidth: Int = super.descrLHwidth + 1 + abbr.length
}
private[console] case class UsagePriorNode(name: String, alias: Seq[String], description: String,
                                           descrOffset: Int = 0) extends UsageNode {
  override def descrLHwidth: Int = super.descrLHwidth + alias.map(_.length + 1).sum
}
private[console] case class UsagePropNode(name: String, flag: String, description: String,
                                          descrOffset: Int = 0) extends UsageNode {
  override def descrLHwidth: Int = super.descrLHwidth + 1 + flag.length
}

private object UsageArgTree {
  implicit val argTree2usageEv: Convertible[ArgTree, UsageArgTree] = (a: ArgTree) => {
    val topParams = a.topParams.map(_.convertTo[UsageParamNode])
    val topOpts = a.topOpts.map(_.convertTo[UsageOptNode])
    val priors = a.priors.map(_.convertTo[UsagePriorNode])
    val props = a.props.map(_.convertTo[UsagePropNode])
    val subCmds = a.cmdEntry.children.map(recConvertCmdNode)
    UsageArgTree(a.appInfo, topParams, topOpts, priors, props, subCmds)
  }

  private def recConvertCmdNode(a: CmdNode): UsageCmdNode = {
    val name = a.entity.originalName
    val description = a.entity.description.map(dscr => s": $dscr").getOrElse("")
    val params = a.params.map(_.convertTo[UsageParamNode])
    val opts = a.opts.map(_.convertTo[UsageOptNode])
    val subCmds = a.subCmdEntry.children.map(recConvertCmdNode)
    UsageCmdNode(name, description, params, opts, subCmds)
  }

  /** Align nodes by start place of description. */
  def align[N <: UsageNode](seq: Seq[N]): Seq[N] = {
    val maxLHwidth = seq.map(_.descrLHwidth).max
    seq.map { n =>
      val offset = maxLHwidth - n.descrLHwidth + 1
      val result = n match {
        case n: UsageParamNode => n.copy(descrOffset = offset)
        case n: UsageOptNode => n.copy(descrOffset = offset)
        case n: UsagePriorNode => n.copy(descrOffset = offset)
        case n: UsagePropNode => n.copy(descrOffset = offset)
      }
      result.asInstanceOf[N]
    }
  }

  implicit def paramNode2usageEv[T]: Convertible[ParamNode[T], UsageParamNode] =
    (a: ParamNode[T]) => {
      val name = s"<${a.entity.originalName}>"
      val description = a.entity.description.map(dscr => s": $dscr").getOrElse("")
      UsageParamNode(name, description, a.entity.isMandatory)
    }

  implicit def optNode2usageEv[T]: Convertible[OptNode[T], UsageOptNode] =
    (a: OptNode[T]) => {
      val abbr = a.entity.originalAbbr.map(ab => s"$ab ").getOrElse("")
      val name = a.entity.originalName
      val description = a.entity.description.map(dscr => s": $dscr").getOrElse("")
      UsageOptNode(abbr, name, description, a.entity.isMandatory)
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