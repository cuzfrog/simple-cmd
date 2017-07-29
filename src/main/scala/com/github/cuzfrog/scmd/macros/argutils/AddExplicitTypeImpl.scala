package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.Defaults
import com.github.cuzfrog.scmd.internal.RawArgMacro

import scala.collection.immutable
import scala.meta._

private object AddExplicitTypeImpl {
  def addExplicitType(stat: Stat): Stat = {
    implicit val pos = stat.pos
    stat match {
      case q"val $cmd:$_ = cmdDef(..$params)" =>
        q"val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = MetaApi.cmdDef"

      case q"val $para:$_ = paramDef[$tpe](..$params)" =>
        getMetaApi(para, Types.parameter, tpe, Types.singleValue, params)

      case q"val $opt:$_ = optDef[$tpe](..$params)" =>
        getMetaApi(opt, Types.optionArg, tpe, Types.singleValue, params)

      case q"val $para:$_ = paramDefVariable[$tpe](..$params)" =>
        getMetaApi(para, Types.parameter, tpe, Types.variableValue, params)

      case q"val $opt:$_ = optDefMultiple[$tpe](..$params)" =>
        getMetaApi(opt, Types.optionArg, tpe, Types.variableValue, params)

      case other => other
    }
  }

  private def getMetaApi(argName: Pat, arg: Type, tpe: Type, argValue: Type,
                         params: immutable.Seq[Term.Arg])
                        (implicit pos: Position): Defn.Val = {
    val isMandatory = RawArgMacro.extract[Boolean](params).getOrElse(Defaults.isMandatory)
    val composedTpe = if (isMandatory) {
      t"$arg[$tpe] with $argValue[$tpe] with Mandatory"
    } else {
      t"$arg[$tpe] with $argValue[$tpe]"
    }
    val metaApi = {
      val mandatory = if (isMandatory) "Mandatory" else ""
      val name = Term.Name(arg.syntax + argValue.syntax + mandatory)
      q"MetaApi.$name[$tpe]"
    }

    q"val ${argName.asInstanceOf[Pat.Var.Term]}:$composedTpe = $metaApi"
  }
}
