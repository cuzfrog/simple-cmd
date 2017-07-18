package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{Command, CommandEntry, OptionArg, Parameter}

import scala.reflect.ClassTag

final case class ArgTree(topParams: Seq[ParamNode[_]],
                         topOpts: Seq[OptNode[_]],
                         cmdEntry: CmdEntryNode) {
  def toTopNode: CmdNode = new CmdNode {
    override val parent: Option[CmdNode] = None
    override def params: Seq[ParamNode[_]] = topParams
    override def opts: Seq[OptNode[_]] = topOpts
    override def subCmdEntry: CmdEntryNode = cmdEntry
    override def entity: Command = Command("AppName", None) //todo: replace AppName
  }
}

trait CmdNode {
  def entity: Command
  def params: Seq[ParamNode[_]]
  def opts: Seq[OptNode[_]]

  def parent: Option[CmdNode]
  def subCmdEntry: CmdEntryNode
}

trait CmdEntryNode {
  val entity: CommandEntry
  val children: Seq[CmdNode]
  lazy val mandatoryCnt:Int = ArgTreeUtils.countMandatory(this)
}

trait NodeTag[+N <: NodeTag[N]]

trait ValueNode {
  def value: Seq[String]
  def tpe: ClassTag[_]
}

case class ParamNode[+T](entity: Parameter[T],
                         tpe: ClassTag[_],
                         value: Seq[String])
  extends ValueNode with NodeTag[ParamNode[T]]

case class OptNode[+T](entity: OptionArg[T],
                       tpe: ClassTag[_],
                       value: Seq[String])
  extends ValueNode with NodeTag[OptNode[T]]

//todo: find out is it able to new private class.