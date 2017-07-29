package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.internal.RawArgMacro
import com.github.cuzfrog.scmd.{Defaults, OptionArg, Parameter}

import scala.meta._
import scala.collection.immutable

private object ArgUtils {
  /** Generate fields of parsed arguments for newly created argDef class. */
  def convertParsed(stats: immutable.Seq[Stat]): immutable.Seq[Stat] = stats collect {
    case q"val $cmd:$_ = cmdDef(..$params)" =>
      q"""override val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = {
            scmdRuntime.getEvaluatedArgumentByName[Boolean,Command](${Lit.String(cmd.syntax)})
          }"""
    case q"val $para:$_ = paramDef[$tpe](..$params)" =>
      typedVal(para, tpe, classOf[Parameter[_]], isVariable = false)
    case q"val $opt:$_ = optDef[$tpe](..$params)" =>
      typedVal(opt, tpe, classOf[OptionArg[_]], isVariable = false)
    case q"val $para:$_ = paramDefVariable[$tpe](..$params)" =>
      typedVal(para, tpe, classOf[Parameter[_]], isVariable = true)
    case q"val $opt:$_ = optDefMultiple[$tpe](..$params)" =>
      typedVal(opt, tpe, classOf[OptionArg[_]], isVariable = true)
  }

  private def typedVal(argName: Pat, tpe: Type,
                       argType: Class[_], isVariable: Boolean): Defn.Val = {
    val name = Type.Name(argType.getSimpleName)
    val argValue = if (isVariable) t"VariableValue[$tpe]" else t"SingleValue[$tpe]"
    q"""override val ${argName.asInstanceOf[Pat.Var.Term]}: $name[$tpe] with $argValue = {
          scmdRuntime
          .getEvaluatedArgumentByName[$tpe,$name[$tpe] with $argValue](${Lit.String(argName.syntax)})
        }"""
  }
  //todo: convert camel case name to hyphen linked

  /** Scala meta generated fields need explicit types to inform IDE. */
  def addExplicitType(stat: Stat): Stat = {
    implicit val pos = stat.pos
    stat match {
      case q"val $cmd:$_ = cmdDef(..$params)" =>
        q"val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = cmdDef(..$params)"

      case q"val $para:$_ = paramDef[$tpe](..$params)" =>
        q"""val ${para.asInstanceOf[Pat.Var.Term]}: Parameter[$tpe] with SingleValue[$tpe] with ${withMandatory(params)} =
            paramDef[$tpe](..$params)"""

      case q"val $opt:$_ = optDef[$tpe](..$params)" =>
        q"""val ${opt.asInstanceOf[Pat.Var.Term]}: OptionArg[$tpe] with SingleValue[$tpe] =
            optDef[$tpe](..$params)"""

      case q"val $para:$_ = paramDefVariable[$tpe](..$params)" =>
        q"""val ${para.asInstanceOf[Pat.Var.Term]}: Parameter[$tpe] with VariableValue[$tpe] with ${withMandatory(params)} =
           MetaApi.paramDefVariableM[$tpe]"""

      case q"val $opt:$_ = optDefMultiple[$tpe](..$params)" =>
        q"""val ${opt.asInstanceOf[Pat.Var.Term]}: OptionArg[$tpe] with VariableValue[$tpe] =
            optDefMultiple[$tpe](..$params)"""

      case other => other
    }
  }

  private def withMandatory(params: immutable.Seq[Term.Arg])(implicit pos: Position): Type = {
    val isMandatory = RawArgMacro.extract[Boolean](params)
    if (isMandatory.getOrElse(Defaults.isMandatory)) {
      t"Mandatory"
    } else {
      t"_"
    }
  }
}
