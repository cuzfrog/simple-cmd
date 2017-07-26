package com.github.cuzfrog.scmd.macros

import scala.meta._

private object ParsedArg {
  /** Generate fields of parsed arguments for newly created argDef class. */
  def convertParsed(stat: Stat): Stat = stat match {
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
    case other => other
  }

  private def stripTpe(tpe: Type): Type = tpe match {
    case t"Seq[$t]" => t
    case t"List[$t]" => t
    case t => t
  }
}
