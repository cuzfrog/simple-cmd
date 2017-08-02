package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.Limitation

import scala.collection.immutable
import scala.meta._

private case class LimitationGroup(limitation: Limitation, args: immutable.Seq[String])
private object LimitationGroup {

  def fromTuple(tuple: (Limitation, immutable.Seq[TermArg], Int)): LimitationGroup =
    LimitationGroup(tuple._1, tuple._2.map(_.name))

  implicit val definableLimitationTuple: Definable[LimitationGroup] = (a: LimitationGroup) => {
    val symbols =q"""Seq(..${a.args.map(name => Lit.Symbol(scala.Symbol(name)))})"""
    q"(Limitation.${Term.Name(a.limitation.toString)},$symbols)"
  }
}