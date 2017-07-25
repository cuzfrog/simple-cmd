package com.github.cuzfrog.scmd.macros

import scala.meta._

private object ParsedArg {
  /** Generate fields of parsed arguments for newly created argDef class. */
  def convertParsed(stat:Stat):Stat = stat match{
    case q"val $cmd:$_ = cmdDef(..$params)" =>
      q"val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = nodes.find(_.entity)"
    case q"val $para:$_ = paramDef[$tpe](..$params)" =>
      q"val ${para.asInstanceOf[Pat.Var.Term]}: Parameter[$tpe] = paramDef[$tpe](..$params)"
    case q"val $opt:$_ = optDef[$tpe](..$params)" =>
      q"val ${opt.asInstanceOf[Pat.Var.Term]}: OptionArg[$tpe] = optDef[$tpe](..$params)"
    case other => other
  }
}
