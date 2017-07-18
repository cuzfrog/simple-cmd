package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.{Command, CommandEntry, OptionArg, Parameter}

import scala.meta._
import scala.collection.immutable

/**
  * Created by cuz on 7/14/17.
  */
private trait TermArg {
  def term: Term
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
      case opt: OptionArg[_] =>
        val isMandatory = Lit.Boolean(opt.isMandatory)
        val abbr = opt.abbr match {
          case Some(s) => q"Option(${Lit.String(s)})"
          case None => q"None"
        }
        val term =
          q"""OptionArg[${rawArg.tpe}]($TERM_NAME = $name,
                                       $TERM_ABBREVIATION = $abbr
                                       $TERM_DESCRIPTION = $description,
                                       $TERM_IS_MANDATORY = $isMandatory)"""
        TermOpt(term, rawArg.idx, rawArg.tpe)

      case _: CommandEntry =>
        throw new AssertionError("CommandEntry will not be in a RawArg.")
    }
  }
}

private final case class TermCmd(term: Term, idx: Int) extends TermArg {val tpe = TYPE_NOTHING}
private final case class TermParam(term: Term, idx: Int, tpe: Type) extends TermArg
private final case class TermOpt(term: Term, idx: Int, tpe: Type) extends TermArg
private final case class TermCommandEntry(term: Term,
                                          children: immutable.Seq[TermCmdNode]) extends TermArg {
  val idx: Int = 0
  val tpe: Type = TYPE_NOTHING
}

private object TermParam {
  implicit val definable: Definable[TermParam] = (a: TermParam) => {
    q"""new com.github.cuzfrog.scmd.parse.ParamNode[${a.tpe}]{
            val entity:Parameter[${a.tpe}] = ${a.term}
            val tpe = _root_.scala.reflect.ClassTag(classOf[${a.tpe}])
        }"""
  }
}

private object TermOpt {
  implicit val definable: Definable[TermOpt] = (a: TermOpt) => {
    q"""new com.github.cuzfrog.scmd.parse.OptNode[${a.tpe}]{
            val entity:Parameter[${a.tpe}] = ${a.term}
            val tpe = _root_.scala.reflect.ClassTag(classOf[${a.tpe}])
        }"""
  }
}

private object TermCommandEntry {
  implicit val definable: Definable[TermCommandEntry] = (a: TermCommandEntry) => {
    val children = a.children match {
      case Nil => q"$TERM_immutable.Seq.empty[CmdNode]"
      case cdren => q"$TERM_immutable.Seq(..${cdren.map(_.defnTerm)})"
    }
    q"""new com.github.cuzfrog.scmd.parse.CmdEntryNode{
          val entity = ${a.term}
          val children = $children
        }"""
  }

  val default: TermCommandEntry = {
    val term =
      q"""CommandEntry("",None)"""
    TermCommandEntry(term = term, children = immutable.Seq.empty)
  }

  def defaultWithCmdNodes(commandNodes: immutable.Seq[TermCmdNode]): TermCommandEntry = {
    this.default.copy(children = commandNodes)
  }
}