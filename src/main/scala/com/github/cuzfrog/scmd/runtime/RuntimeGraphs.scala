package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.{Argument, Command, OptionArg, Parameter}

import scala.reflect.ClassTag

private trait Node[+A <: Argument[T], T] {
  def entity: A
  //def value: Option[T]
  def tpe: ClassTag[T]
}

private trait CmdNode extends Node[Command, Nothing] {
  val tpe = ClassTag(classOf[Nothing])
  val value = None
  def params: Seq[ParamNode[_]]
  def opts: Seq[OptNode[_]]
  def children: Seq[CmdNode]
}
private trait ParamNode[T] extends Node[Parameter[T], T]
private trait OptNode[T] extends Node[OptionArg[T], T]

private final case class ArgGraph(commands: Seq[CmdNode],
                                  topParams: Seq[ParamNode[_]],
                                  topOpts: Seq[OptNode[_]])
