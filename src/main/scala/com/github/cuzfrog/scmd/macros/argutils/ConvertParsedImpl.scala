package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.Argument
import com.github.cuzfrog.scmd.internal.SimpleLogging

import scala.collection.immutable
import scala.meta._

/**
  * Macro implementation to convert client def class statements.
  */
private object ConvertParsedImpl extends SimpleLogging {
  override protected val loggerLevel: SimpleLogging.Level = SimpleLogging.Info

  def convertParsed(stats: immutable.Seq[Stat]): immutable.Seq[Stat] = {
    val collected = stats collect {
      case q"val $cmd:$_ = cmdDef(..$params)" =>
        q"""override val ${cmd.asInstanceOf[Pat.Var.Term]}: Command = {
            scmdRuntime.getEvaluatedArgumentByName
            [Boolean,Command](${Lit.Symbol(scala.Symbol(cmd.syntax))})
          }"""
      case q"val $prior:$_ = priorDef(..$params)" =>
        q"""override val ${prior.asInstanceOf[Pat.Var.Term]}: PriorArg = {
            scmdRuntime.getEvaluatedArgumentByName
            [Boolean,PriorArg](${Lit.Symbol(scala.Symbol(prior.syntax))})
          }"""

      case stat@q"val $para:$_ = paramDef[$tpe](..$params)" =>
        typedVal(para, Types.parameter, tpe, Types.singleValue, params, stat)
      case stat@q"val $opt:$_ = optDef[$tpe](..$params)" =>
        typedVal(opt, Types.optionArg, tpe, Types.singleValue, params, stat)
      case stat@q"val $para:$_ = paramDefVariable[$tpe](..$params)" =>
        typedVal(para, Types.parameter, tpe, Types.variableValue, params, stat)
      case stat@q"val $opt:$_ = optDefVariable[$tpe](..$params)" =>
        typedVal(opt, Types.optionArg, tpe, Types.variableValue, params, stat)
      case stat@q"val $opt:$_ = propDef[$tpe](..$params)" =>
        typedVal(opt, Types.propertyArg, tpe, Types.variableValue, params, stat)
    }
    builtInPriors ++ collected
  }

  private def builtInPriors: immutable.Seq[Defn.Val] = Argument.builtInArgs.map {
    case (symbol, _) =>
      q"""override val ${Pat.Var.Term(Term.Name(symbol.name))}: PriorArg = {
            scmdRuntime.getEvaluatedArgumentByName[Nothing,PriorArg](${Lit.Symbol(symbol)})
        }"""
  }.to[immutable.Seq]

  private def typedVal(argName: Pat, arg: Type, tpe: Type, argValue: Type,
                       params: immutable.Seq[Term.Arg], stat: Stat): Defn.Val = {
    implicit val pos: Position = stat.pos
    val (_, _, composedTpe) = ArgUtils.getComposedTpe(params, arg, tpe, argValue)
    debug(s"GetEvaluatedArument $argName of type[$composedTpe]")
    q"""override val ${argName.asInstanceOf[Pat.Var.Term]}: $composedTpe = {
          scmdRuntime.getEvaluatedArgumentByName
          [$tpe,$composedTpe](${Lit.Symbol(scala.Symbol(argName.syntax))})
        }"""
  }
}
