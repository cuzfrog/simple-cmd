package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

trait CmdNode {
  def entity: Command
  def params: Seq[ParamNode[_]]
  def opts: Seq[OptNode[_]]

  def parent: Option[CmdNode]
  def subCmdEntry: CommandEntryNode
}

trait CommandEntryNode {
  val entity: CommandEntry
  val children: Seq[CmdNode]
}

trait NodeTag[+N <: NodeTag[N]]

trait ValueNode {
  def value: Option[String]
  def tpe: ClassTag[_]
}

case class ParamNode[+T](entity: Parameter[T],
                         tpe: ClassTag[_],
                         value: Option[String])
  extends ValueNode with NodeTag[ParamNode[T]]

case class OptNode[+T](entity: OptionArg[T],
                       tpe: ClassTag[_],
                       value: Option[String])
  extends ValueNode with NodeTag[OptNode[T]]

final case class ArgTree(topParams: Seq[ParamNode[_]],
                         topOpts: Seq[OptNode[_]],
                         cmdEntry: CommandEntryNode) {
  def toTopNode: CmdNode = new CmdNode {
    override val parent: Option[CmdNode] = None
    override def params: Seq[ParamNode[_]] = topParams
    override def opts: Seq[OptNode[_]] = topOpts
    override def subCmdEntry: CommandEntryNode = cmdEntry
    override def entity: Command = Command("AppName", None) //todo: replace AppName
  }
}

//todo: find out is it able to new private class.