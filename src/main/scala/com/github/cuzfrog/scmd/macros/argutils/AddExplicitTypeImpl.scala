package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.macros.argutils.RawArg.{RawCommand, RawPrior}

import scala.collection.immutable
import scala.meta._

private object AddExplicitTypeImpl {
  def addExplicitType(rawArgs: immutable.Seq[RawArg]): immutable.Seq[Stat] = {
    rawArgs map {
      case r: RawCommand =>
        q"val ${r.name.toPatTerm}: Command = DummyApi.cmdDef"

      case r: RawPrior =>
        q"val ${r.name.toPatTerm}: PriorArg = DummyApi.priorDef"

      case r: RawTypedArg =>
        q"val ${r.name.toPatTerm}:${r.composedTpe} = ${getDummyApi(r.tpe, r.composedTpe)}"
    }
  }

  private def getDummyApi(tpe: Type, composedTpe: Type): Term.ApplyType = {
    val name = Term.Name(composedTpe.syntax.replaceAll("""(with)|(\s)|(\[[^\[\]]+\])""", ""))
    q"DummyApi.$name[$tpe]"
  }
}
