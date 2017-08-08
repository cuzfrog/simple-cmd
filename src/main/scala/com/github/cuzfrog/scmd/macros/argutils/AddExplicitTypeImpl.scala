package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.Defaults
import com.github.cuzfrog.scmd.internal.RawArgMacro

import scala.collection.immutable
import scala.meta._

private object AddExplicitTypeImpl {
  def addExplicitType(rawArgs: immutable.Seq[RawArg]): immutable.Seq[Stat] = {
    rawArgs map {
      case q"val $cmd:$_ = cmdDef(..$params)" =>
        q"val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = DummyApi.cmdDef"

      case q"val $para:$_ = paramDef[$tpe](..$params)" =>
        getDummyApi(para, Types.parameter, tpe, Types.singleValue, params)

      case q"val $opt:$_ = optDef[$tpe](..$params)" =>
        getDummyApi(opt, Types.optionArg, tpe, Types.singleValue, params)

      case q"val $para:$_ = paramDefVariable[$tpe](..$params)" =>
        getDummyApi(para, Types.parameter, tpe, Types.variableValue, params)

      case q"val $opt:$_ = optDefVariable[$tpe](..$params)" =>
        getDummyApi(opt, Types.optionArg, tpe, Types.variableValue, params)

      case q"val $prop:$_ = propDef[$tpe](..$params)" =>
        getDummyApi(prop, Types.propertyArg, tpe, Types.variableValue, params)

      case q"val $prior:$_ = priorDef(..$params)" =>
        q"val ${prior.asInstanceOf[Pat.Var.Term]}: PriorArg = DummyApi.priorDef"

      case other => other
    }
  }

  private def getDummyApi(argName: Pat, arg: Type, tpe: Type, argValue: Type,
                          params: immutable.Seq[Term.Arg]): Defn.Val = {
    implicit val pos: Position = argName.pos
    val (isMandatory, withDefault, composedTpe) =
      ArgUtils.getComposedTpe(params, arg, tpe, argValue)
    val metaApi = {
      val mandatory = if (isMandatory) "Mandatory" else ""
      val default = if (withDefault) "Default" else ""
      val name = Term.Name(arg.syntax + argValue.syntax + mandatory + default)
      q"DummyApi.$name[$tpe]"
    }

    q"val ${argName.asInstanceOf[Pat.Var.Term]}:$composedTpe = $metaApi"
  }
}
