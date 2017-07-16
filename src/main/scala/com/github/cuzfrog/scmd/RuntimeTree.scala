package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

trait Node[+A <: Argument[T], +T] {
  def entity: A
}

trait CmdNode extends Node[Command, Nothing] {
  def params: Seq[ParamNode[_]]
  def opts: Seq[OptNode[_]]

  def parent: Option[CmdNode]
  def children: Seq[CmdNode]
}

trait NodeTag[+N <: NodeTag[N]]

trait ValueNode {
  def value: Option[String]
  def tpe: ClassTag[_]
}

case class ParamNode[+T](entity: Parameter[T],
                        tpe: ClassTag[_],
                        value: Option[String])
  extends Node[Parameter[T], T] with ValueNode with NodeTag[ParamNode[T]]

case class OptNode[+T](entity: OptionArg[T],
                      tpe: ClassTag[_],
                      value: Option[String])
  extends Node[OptionArg[T], T] with ValueNode with NodeTag[OptNode[T]]

final case class ArgTree(commands: Seq[CmdNode],
                         topParams: Seq[ParamNode[_]],
                         topOpts: Seq[OptNode[_]]) {
  def toTopNode: CmdNode = new CmdNode {
    override val parent: Option[CmdNode] = None
    override def params: Seq[ParamNode[_]] = topParams
    override def opts: Seq[OptNode[_]] = topOpts
    override def children: Seq[CmdNode] = commands
    override def entity: Command = Command("AppName", None)
  }
}

//todo: find out is it able to new private class.