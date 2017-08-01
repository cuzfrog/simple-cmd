package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.Defaults
import com.github.cuzfrog.scmd.internal.{RawArgMacro, SimpleLogging}
import com.github.cuzfrog.scmd.macros.Constants._

import scala.collection.immutable
import scala.meta._

/**
  * Created by cuz on 7/14/17.
  */
private trait TermArg {
  def name: String
  def term: Term
  def pos: Position
  def tpe: Type
}
private object TermArg extends SimpleLogging {
  override implicit val loggerLevel = SimpleLogging.Info
  def collectTermArg(stats: immutable.Seq[Stat]): immutable.Seq[TermArg] = {
    import RawArgMacro.extract

    stats zip stats.map(_.pos) collect {
      case (q"val $cmd: $_ = cmdDef(..$params)", pos) =>
        implicit val position = pos
        val description = extract[String](params).defnTerm
        val term =
          q"""runtime.buildCommand($TERM_NAME = ${Lit.String(cmd.syntax)},
                                   $TERM_DESCRIPTION = $description)"""
        TermCmd(cmd.syntax, term, pos)
      case (q"val $argName: $_ = $defName[$tpe](..$params)", pos) =>
        debug(s"Collected type of $argName is [$tpe]")
        implicit val position = pos
        val description = extract[String](params).defnTerm
        val isMandatory = extract[Boolean](params).getOrElse(Defaults.isMandatory).defnTerm
        val abbr = extract[String](params).defnTerm
        val default = extract[Term.Arg](params).defnTerm
        val name = Lit.String(argName.syntax)
        defName match {
          case q"paramDef" =>
            val term =
              q"""runtime.buildParameter[$tpe]($TERM_NAME = $name,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatory,
                                               argValue = ${singleValue(tpe, default)})"""
            TermParam(argName.syntax, term, pos, tpe)
          case q"paramDefVariable" =>
            val term =
              q"""runtime.buildParameter[$tpe]($TERM_NAME = $name,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatory,
                                               argValue = ${variableValue(tpe, default)})"""
            TermParam(argName.syntax, term, pos, tpe)
          case q"optDef" =>
            val term =
              q"""runtime.buildOptionArg[$tpe]($TERM_NAME = $name,
                                               $TERM_ABBREVIATION = $abbr,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatory,
                                               argValue = ${singleValue(tpe, default)})"""
            TermOpt(argName.syntax, term, pos, tpe)
          case q"optDefMultiple" =>
            val term =
              q"""runtime.buildOptionArg[$tpe]($TERM_NAME = $name,
                                               $TERM_ABBREVIATION = $abbr,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatory,
                                               argValue = ${variableValue(tpe, default)})"""
            TermOpt(argName.syntax, term, pos, tpe)

        }
    }
  }

  private def singleValue(tpe: Type, default: Term): Term =
    q"""runtime.buildSingleValue[$tpe]($default)"""
  private def variableValue(tpe: Type, default: Term): Term =
    q"""runtime.buildVariableValue[$tpe]($default.toSeq.flatten)"""
}

private sealed trait TermValueArg extends TermArg
private final
case class TermCmd(name: String, term: Term, pos: Position) extends TermArg {val tpe = TYPE_NOTHING}
private final
case class TermParam(name: String, term: Term, pos: Position, tpe: Type) extends TermValueArg
private final
case class TermOpt(name: String, term: Term, pos: Position, tpe: Type) extends TermValueArg
private final
case class TermCommandEntry(term: Term, children: immutable.Seq[TermCmdNode])

private object TermCmd {
  val dummy: TermCmd = TermCmd("", q"_", Position.None)
}

private object TermParam {
  implicit val definable: Definable[TermParam] = (a: TermParam) => {
    q"""runtime.buildParamNode[${a.tpe}](
            entity = ${a.term},
            value = Nil
        )"""
  }
}

private object TermOpt {
  implicit val definable: Definable[TermOpt] = (a: TermOpt) => {
    q"""runtime.buildOptNode[${a.tpe}](
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
    q"""runtime.buildCmdEntryNode(
          entity = ${a.term},
          children = $children
        )"""
  }

  val default: TermCommandEntry = {
    val term =
      q"""runtime.defaultCommandEntry"""
    TermCommandEntry(term = term, children = immutable.Seq.empty)
  }

  def defaultWithCmdNodes(commandNodes: immutable.Seq[TermCmdNode]): TermCommandEntry = {
    this.default.copy(children = commandNodes)
  }
}