package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.{Argument, Command, OptionArg, Parameter}

import scala.meta.{Term, Type}
import scala.reflect.ClassTag

private trait Node[+A <: Argument[_]] {
  def entity: A
  def tpe: ClassTag[_]

}
private trait CmdNode extends Node[Command]
private trait ParamNode extends Node[Parameter[_]]
private trait OptNode extends Node[OptionArg[_]]
private final case class ArgGraph(commands: Seq[CmdNode], opts: Seq[OptNode])

// ============ Raw(contains macro type) ==============
private trait ArgType
private object ArgType {
  case object Cmd extends ArgType
  case object Param extends ArgType
  case object Opt extends ArgType
}

private final case class RawArg(arg: Argument[_], idx: Int, tpe: Type)
private final case class TermArg(arg: Term, argType: ArgType, idx: Int, tpe: Type)

private final case class RawCmdNode(cmd: Command,
                                    params: Seq[(Parameter[_], Type)],
                                    opts: Seq[(OptionArg[_], Type)],
                                    children: Seq[RawCmdNode])

private final case class RawArgGraph(commands: Seq[RawCmdNode], opts: Seq[(OptionArg[_], Type)])
