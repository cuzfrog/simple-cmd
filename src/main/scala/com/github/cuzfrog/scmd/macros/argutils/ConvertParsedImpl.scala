package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.Defaults
import com.github.cuzfrog.scmd.internal.RawArgMacro

import scala.collection.immutable
import scala.meta._

private object ConvertParsedImpl {
  def convertParsed(stats: immutable.Seq[Stat]): immutable.Seq[Stat] = {
    stats collect {
      case q"val $cmd:$_ = cmdDef(..$params)" =>
        q"""override val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = {
            scmdRuntime.getEvaluatedArgumentByName[Boolean,Command](${Lit.String(cmd.syntax)})
          }"""
      case stat@q"val $para:$_ = paramDef[$tpe](..$params)" =>
        typedVal(para, Types.parameter, tpe, Types.singleValue, params, stat)
      case stat@q"val $opt:$_ = optDef[$tpe](..$params)" =>
        typedVal(opt, Types.optionArg, tpe, Types.singleValue, params, stat)
      case stat@q"val $para:$_ = paramDefVariable[$tpe](..$params)" =>
        typedVal(para, Types.parameter, tpe, Types.variableValue, params, stat)
      case stat@q"val $opt:$_ = optDefMultiple[$tpe](..$params)" =>
        typedVal(opt, Types.optionArg, tpe, Types.variableValue, params, stat)
    }
  }

  private def typedVal(argName: Pat, arg: Type, tpe: Type, argValue: Type,
                       params: immutable.Seq[Term.Arg], stat: Stat): Defn.Val = {
    implicit val pos = stat.pos
    val isMandatory = RawArgMacro.extract[Boolean](params).getOrElse(Defaults.isMandatory)
    val composedTpe = if (isMandatory) {
      t"$arg[$tpe] with $argValue[$tpe] with Mandatory"
    } else {
      t"$arg[$tpe] with $argValue[$tpe]"
    }
    q"""override val ${argName.asInstanceOf[Pat.Var.Term]}: $composedTpe = {
          scmdRuntime.getEvaluatedArgumentByName[$tpe,$composedTpe](${Lit.String(argName.syntax)})
        }"""
  }
}
