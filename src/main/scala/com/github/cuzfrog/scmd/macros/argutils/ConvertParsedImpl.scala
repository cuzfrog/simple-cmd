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
        q"""override val ${r.name.toPatTerm}: Command = {
            scmdRuntime.getEvaluatedArgumentByName
            [Boolean,Command](${Lit.Symbol(scala.Symbol(r.name))})
          }"""
      case r: RawPrior =>
        q"""override val ${r.name.toPatTerm}: PriorArg = {
            scmdRuntime.getEvaluatedArgumentByName
            [Boolean,PriorArg](${Lit.Symbol(scala.Symbol(r.name))})
          }"""

      case r: RawTypedArg =>
        typedVal(r.name.toPatTerm, r.tpe, r.composedTpe)
    }
    builtInPriors ++ parsed
  }

  private def builtInPriors: immutable.Seq[Defn.Val] = Argument.builtInArgs.map {
    case (symbol, _) =>
      q"""override val ${Pat.Var.Term(Term.Name(symbol.name))}: PriorArg = {
            scmdRuntime.getEvaluatedArgumentByName[Nothing,PriorArg](${Lit.Symbol(symbol)})
        }"""
  }.to[immutable.Seq]

  private def typedVal(argName: Pat, tpe: Type, composedTpe: Type): Defn.Val = {
    implicit val pos: Position = argName.pos
    debug(s"GetEvaluatedArument $argName of type[$composedTpe]")
    q"""override val ${argName.asInstanceOf[Pat.Var.Term]}: $composedTpe = {
          scmdRuntime.getEvaluatedArgumentByName
          [$tpe,$composedTpe](${Lit.Symbol(scala.Symbol(argName.syntax))})
        }"""
  }
}
