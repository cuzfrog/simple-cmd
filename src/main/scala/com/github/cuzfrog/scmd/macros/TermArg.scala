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
  def pos: Position
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
        val term =
          q"scmdRuntime.buildCommand($TERM_NAME = $name,$TERM_DESCRIPTION = $description)"
        TermCmd(term, rawArg.pos)
      case param: Parameter[_] =>
        val isMandatory = Lit.Boolean(param.isMandatory)
        val term =
          q"""scmdRuntime.buildParameter[${rawArg.tpe}]($TERM_NAME = $name,
                                     $TERM_DESCRIPTION = $description,
                                     $TERM_IS_MANDATORY = $isMandatory)"""
        TermParam(term, rawArg.pos, rawArg.tpe)
      case opt: OptionArg[_] =>
        val isMandatory = Lit.Boolean(opt.isMandatory)
        val abbr = opt.abbr match {
          case Some(s) => q"Option(${Lit.String(s)})"
          case None => q"None"
        }
        val term =
          q"""scmdRuntime.buildOptionArg[${rawArg.tpe}]($TERM_NAME = $name,
                                       $TERM_ABBREVIATION = $abbr,
                                       $TERM_DESCRIPTION = $description,
                                       $TERM_IS_MANDATORY = $isMandatory)"""
        TermOpt(term, rawArg.pos, rawArg.tpe)

      case _: CommandEntry =>
        throw new AssertionError("CommandEntry will not be in a RawArg.")
    }
  }
}

private final case class TermCmd(term: Term, pos: Position) extends TermArg {val tpe = TYPE_NOTHING}
private final case class TermParam(term: Term, pos: Position, tpe: Type) extends TermArg
private final case class TermOpt(term: Term, pos: Position, tpe: Type) extends TermArg
private final case class TermCommandEntry(term: Term,
                                          children: immutable.Seq[TermCmdNode]) extends TermArg {
  val pos: Position = Position.None
  val tpe: Type = TYPE_NOTHING
}

private object TermParam {
  implicit val definable: Definable[TermParam] = (a: TermParam) => {
    if (a.tpe.syntax.count(_ == '[') > 1) abort(a.pos,
      s"Nested type[${a.tpe}] is not supported. " +
        "For variable argument, use Seq[YourType] or List[YourType]. ")
    val (tpe, isVariable) = a.tpe match {
      case t"Seq[$t]" => (t, true)
      case t"List[$t]" => (t, true)
      case t"Option[$t]" =>
        abort(a.pos, s"[${a.tpe}]," +
          s" Option is unnecessary, optional arg is wrapped with Option automatically.")
      case t => (t, false)
    }

    q"""scmdRuntime.buildParamNode[${a.tpe}](
            entity = ${a.term},
            value = Nil,
            isVariable = ${Lit.Boolean(isVariable)},
            tpe = _root_.scala.reflect.ClassTag(classOf[$tpe])
        )"""
  }
}

private object TermOpt {
  implicit val definable: Definable[TermOpt] = (a: TermOpt) => {
    q"""scmdRuntime.buildOptNode[${a.tpe}](
            entity = ${a.term},
            value = Nil
        )"""
  }
}

private object TermCommandEntry {
  implicit val definable: Definable[TermCommandEntry] = (a: TermCommandEntry) => {
    val children = a.children match {
      case Nil => q"$TERM_immutable.Seq.empty[Int]"
      case cdren => q"$TERM_immutable.Seq(..${cdren.map(_.defnTerm)})"
    }
    q"""scmdRuntime.buildCmdEntryNode(
          entity = ${a.term},
          children = $children
        )"""
  }

  val default: TermCommandEntry = {
    val term =
      q"""scmdRuntime.defaultCommandEntry"""
    TermCommandEntry(term = term, children = immutable.Seq.empty)
  }

  def defaultWithCmdNodes(commandNodes: immutable.Seq[TermCmdNode]): TermCommandEntry = {
    this.default.copy(children = commandNodes)
  }
}