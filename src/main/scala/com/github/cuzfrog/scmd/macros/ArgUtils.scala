package com.github.cuzfrog.scmd.macros

import scala.meta._

import scala.collection.immutable

private object ArgUtils {
  /** Generate fields of parsed arguments for newly created argDef class. */
  def convertParsed(stats: immutable.Seq[Stat]): immutable.Seq[Stat] = stats collect {
    case q"val $cmd:$_ = cmdDef(..$params)" =>
      q"""override val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = {
            scmdRuntime.getArgumentWithValueByName[Boolean,Command](${Lit.String(cmd.syntax)})
          }"""
    case q"val $para:$_ = paramDef[$tpe](..$params)" =>
      q"""override val ${para.asInstanceOf[Pat.Var.Term]}: Parameter[$tpe] = {
            scmdRuntime
            .getArgumentWithValueByName[$tpe,Parameter[$tpe]](${Lit.String(para.syntax)})
          }"""
    case q"val $opt:$_ = optDef[$tpe](..$params)" =>
      q"""override val ${opt.asInstanceOf[Pat.Var.Term]}: OptionArg[$tpe] = {
            scmdRuntime
            .getArgumentWithValueByName[$tpe,OptionArg[$tpe]](${Lit.String(opt.syntax)})
         }"""
  }

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
