package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{CmdNode, CommandEntryNode, OptNode, ParamNode}

private case class StateCmdNode(ref: CmdNode,
                                params: Seq[StateParamNode],
                                opts: Seq[StateOptNode],
                                parent: Option[StateCmdNode],
                                subCmdEntry: StateCmdEntryNode)

private trait StateValueNode[T <: StateValueNode[T]] extends StringUtils {
  private[this] var value: Option[String] = None

  def setValue(v: String): StateValueNode[T] = {
    this.value = v
    this
  }

  def getValue: Option[String] = value
}

private case class StateParamNode(ref: ParamNode[_]) extends StateValueNode[ParamNode[_]]
private case class StateOptNode(ref: OptNode[_]) extends StateValueNode[OptNode[_]]
private case class StateCmdEntryNode(ref: CommandEntryNode, children: Seq[StateCmdNode])