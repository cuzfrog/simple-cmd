package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

private trait Node[+A <: Argument[_]] {
  def entity: A
  def tpe: ClassTag[_]
}

private trait CmdNode extends Node[Command]
private trait ParamNode extends Node[Parameter[_]]
private trait OptNode extends Node[OptionArg[_]]

private final case class ArgGraph(commands: Seq[CmdNode], opts: Seq[OptNode])
