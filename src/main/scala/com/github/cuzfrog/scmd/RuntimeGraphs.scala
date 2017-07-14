package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

trait Node[+A <: Argument[T], T] {
  def entity: A
  //def value: Option[T]
  def tpe: ClassTag[T]
}

trait CmdNode extends Node[Command, Nothing] {
  val tpe = ClassTag(classOf[Nothing])
  val value = None
  def params: Seq[ParamNode[_]]
  def opts: Seq[OptNode[_]]
  def children: Seq[CmdNode]
}
trait ParamNode[T] extends Node[Parameter[T], T]
trait OptNode[T] extends Node[OptionArg[T], T]

final case class ArgGraph(commands: Seq[CmdNode],
                                  topParams: Seq[ParamNode[_]],
                                  topOpts: Seq[OptNode[_]])
