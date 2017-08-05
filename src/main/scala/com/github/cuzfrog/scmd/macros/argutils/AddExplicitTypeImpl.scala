package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.Defaults
import com.github.cuzfrog.scmd.internal.RawArgMacro

import scala.collection.immutable
import scala.meta._

private object AddExplicitTypeImpl {
  def addExplicitType(stat: Stat): Stat = {
    implicit val pos: Position = stat.pos
    stat match {
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
      //todo: is it necessary to add for props?

      case other => other
    }
  }

  private def getDummyApi(argName: Pat, arg: Type, tpe: Type, argValue: Type,
                         params: immutable.Seq[Term.Arg])
                        (implicit pos: Position): Defn.Val = {
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
