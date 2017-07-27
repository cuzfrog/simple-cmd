package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.internal.RawArgMacro
import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.{Argument, Command, Defaults, OptionArg, Parameter}

import scala.collection.immutable
import scala.meta._

private final case class RawArg(arg: Argument[_], pos: Position, tpe: Type)

private object RawArg {
  //todo: convert camel case name to hyphen linked

  /** Scala meta generated fields need explicit types to inform IDE. */
  def addExplicitType(stat: Stat): Stat = stat match {
    case q"val $cmd:$_ = cmdDef(..$params)" =>
      q"val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = cmdDef(..$params)"
    case q"val $para:$_ = paramDef[$tpe](..$params)" =>
      q"val ${para.asInstanceOf[Pat.Var.Term]}: Parameter[$tpe] = paramDef[$tpe](..$params)"
    case q"val $opt:$_ = optDef[$tpe](..$params)" =>
      q"val ${opt.asInstanceOf[Pat.Var.Term]}: OptionArg[$tpe] = optDef[$tpe](..$params)"
    case other => other
  }
}