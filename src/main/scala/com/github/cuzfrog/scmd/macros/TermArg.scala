package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.macros.Constents._
import com.github.cuzfrog.scmd.{Command, OptionArg, Parameter}

import scala.meta._

/**
  * Created by cuz on 7/14/17.
  */
private trait TermArg {
  def arg: Term
  def idx: Int
  def tpe: Type
}
private object TermArg {
  def raw2termArg(rawArg: RawArg): TermArg = {
    val name = Lit.String(rawArg.arg.name)
    val description = rawArg.arg.description match {
      case Some(dscr) => q"Option(${Lit.String(dscr)})"
      case None => q"None"
    }
    rawArg.arg match {
      case _: Command =>
        val term = q"Command($TERM_NAME = $name,$TERM_DESCRIPTION = $description)"
        TermCmd(term, rawArg.idx)
      case param: Parameter[_] =>
        val isMandatory = Lit.Boolean(param.isMandatory)
        val term =
          q"""Parameter[${rawArg.tpe}]($TERM_NAME = $name,
                                     $TERM_DESCRIPTION = $description,
                                     $TERM_IS_MANDATORY = $isMandatory)"""
        TermParam(term, rawArg.idx, rawArg.tpe)
      case _: OptionArg[_] =>
        val term = q"OptionArg[${rawArg.tpe}]($TERM_NAME = $name,$TERM_DESCRIPTION = $description)"
        TermOpt(term, rawArg.idx, rawArg.tpe)
    }
  }


}

private final case class TermCmd(arg: Term, idx: Int) extends TermArg {val tpe = TYPE_NOTHING}
private final case class TermParam(arg: Term, idx: Int, tpe: Type) extends TermArg
private final case class TermOpt(arg: Term, idx: Int, tpe: Type) extends TermArg

private object TermParam {
  implicit val definable: Definable[TermParam] = (a: TermParam) => {
    q"""new com.github.cuzfrog.scmd.ParamNode[${a.tpe}]{
            val entity:Parameter[${a.tpe}] = ${a.arg}
            val tpe = _root_.scala.reflect.ClassTag(classOf[${a.tpe}])
        }"""
  }
}

private object TermOpt {
  implicit val definable: Definable[TermOpt] = (a: TermOpt) => {
    q"""new com.github.cuzfrog.scmd.OptNode[${a.tpe}]{
            val entity:Parameter[${a.tpe}] = ${a.arg}
            val tpe = _root_.scala.reflect.ClassTag(classOf[${a.tpe}])
        }"""
  }
}