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

  import RawArg._
  import com.github.cuzfrog.scmd.macros.DefineTermOps

  def convertParsed(rawArgs: immutable.Seq[RawArg]): immutable.Seq[Stat] = {
    val parsed = rawArgs map {
      case r: RawCommand =>
        q"""override val ${r.name}: Command = {
            scmdRuntime.getEvaluatedArgumentByName
            [Boolean,Command](${Lit.Symbol(scala.Symbol(r.name))})
          }"""
      case r: RawPrior =>
        q"""override val ${r.name}: PriorArg = {
            scmdRuntime.getEvaluatedArgumentByName
            [Boolean,PriorArg](${Lit.Symbol(scala.Symbol(r.name))})
          }"""

      case r: RawParam if !r.isVariable =>
        typedVal(r.name, Types.parameter, r.tpe, Types.singleValue, r.isMandatory, r.hasDefault)
      case r: RawOpt if !r.isVariable =>
        typedVal(r.name, Types.optionArg, r.tpe, Types.singleValue, r.isMandatory, r.hasDefault)
      case r: RawParam if r.isVariable =>
        typedVal(r.name, Types.parameter, r.tpe, Types.variableValue, r.isMandatory, r.hasDefault)
      case r: RawOpt if r.isVariable =>
        typedVal(r.name, Types.optionArg, r.tpe, Types.variableValue, r.isMandatory, r.hasDefault)
      case r: RawProp =>
        typedVal(r.name, Types.propertyArg, r.tpe, Types.variableValue,
          isMandatory = false, hasDefault = r.hasDefault)
    }
    builtInPriors ++ parsed
  }

  private def builtInPriors: immutable.Seq[Defn.Val] = Argument.builtInArgs.map {
    case (symbol, _) =>
      q"""override val ${Pat.Var.Term(Term.Name(symbol.name))}: PriorArg = {
            scmdRuntime.getEvaluatedArgumentByName[Nothing,PriorArg](${Lit.Symbol(symbol)})
        }"""
  }.to[immutable.Seq]

  private def typedVal(argName: Pat, arg: Type, tpe: Type, argValue: Type,
                       isMandatory: Boolean, hasDefault: Boolean): Defn.Val = {
    implicit val pos: Position = argName.pos
    val (_, _, composedTpe) = ArgUtils.getComposedTpe(isMandatory, hasDefault, arg, tpe, argValue)
    debug(s"GetEvaluatedArument $argName of type[$composedTpe]")
    q"""override val ${argName.asInstanceOf[Pat.Var.Term]}: $composedTpe = {
          scmdRuntime.getEvaluatedArgumentByName
          [$tpe,$composedTpe](${Lit.Symbol(scala.Symbol(argName.syntax))})
        }"""
  }

  private implicit def string2PatTerm(in: String): Pat.Var.Term = Pat.Var.Term(Term.Name(in))
}
