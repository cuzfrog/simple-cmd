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

sealed trait Node

trait CmdNode extends Node {
  def entity: Command
  def params: Seq[ParamNode[_]]
  def opts: Seq[OptNode[_]]

  def parent: Option[CmdNode]
  def subCmdEntry: CmdEntryNode
}

trait CmdEntryNode extends Node {
  val entity: CommandEntry
  val children: Seq[CmdNode]
  lazy val mandatoryDownstreamCnt: Int = this.countMandatoryDownstream
}

trait NodeTag[+N <: NodeTag[N]]

trait ValueNode extends Node {
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
  extends ValueNode with NodeTag[OptNode[T]] {

  //OptNode's equality depends on its entity's. Value is stripped off for parsing quick comparing.
  override def hashCode(): Int = entity.hashCode * 3 + 17
  override def equals(obj: scala.Any): Boolean = {
    if (!this.canEqual(obj)) return false
    obj.asInstanceOf[OptNode].entity == this.entity
  }
  override def canEqual(that: Any): Boolean = that match {
    case _: OptNode[_] => true
    case _ => false
  }
  //todo: check if equals' overriding is correct.
}

//todo: find out is it able to new private class.