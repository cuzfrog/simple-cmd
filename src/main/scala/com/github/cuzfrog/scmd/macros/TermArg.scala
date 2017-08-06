package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.{Command, Defaults}
import com.github.cuzfrog.scmd.internal.{RawArgMacro, SimpleLogging}
import com.github.cuzfrog.scmd.macros.Constants._

import scala.collection.immutable
import scala.meta._

/*
 * Position is not available, this is a known issue.
 * todo: still try to add position info for client.
 */

/**
  * Created by cuz on 7/14/17.
  */
private sealed trait TermArg {
  def name: String
  def term: Term
  def pos: Position
  def tpe: Type
}
private object TermArg extends SimpleLogging {
  override protected val loggerLevel: SimpleLogging.Level = SimpleLogging.Info
  def collectTermArg(stats: immutable.Seq[Stat], appName: String): immutable.Seq[TermArg] = {
    import RawArgMacro.extract

    val topCmdSymbol = Lit.Symbol(Command.topCmd(appName).symbol)

    stats collect {
      case q"val $argName: $_ = $defName(..$params)" if defName.isInstanceOf[Term.Name] =>
        implicit val pos: Position = argName.pos
        val name = Lit.String(argName.syntax)
        val description = extract[String](params).defnTerm
        defName match {
          case q"cmdDef" =>
            if (!name.value.matches("""\w+.*""")) abort(pos, "Command can only start with letter.")
            val term =
              q"""runtime.buildCommand($TERM_NAME = $name,
                                       $TERM_DESCRIPTION = $description)"""
            TermCmd(name.value, term, pos)
          case q"priorDef" =>
            val alias = extract[Term](params)
            val matchName = extract[Boolean](params).getOrElse(Defaults.priorMatchName)
            if(alias.isEmpty && !matchName)
              abort(pos,s"PriorArg ${name.value} can never be matched," +
                s" no alias defined and not going to match name.")
            val term =
              q"""runtime.buildPriorArg($TERM_NAME = $name,
                                        alias = ${alias.getOrElse(q"Nil")},
                                        $TERM_DESCRIPTION = $description,
                                        matchName = ${matchName.defnTerm})"""
            TermPrior(name.value, term, pos, topCmdSymbol)
        }
      case q"val $argName: $_ = $defName[$tpe](..$params)" =>
        debug(s"Collected $defName: $argName[$tpe]")
        implicit val pos: Position = argName.pos
        val description = extract[String](params).defnTerm
        val isMandatory = extract[Boolean](params).getOrElse(Defaults.isMandatory)
        val isMandatoryTerm = isMandatory.defnTerm
        val abbr = extract[String](params).map {
          case ab if ab.matches("""\w+""") => ab
          case bad => abort(pos, s"Opt abbreviation can only contain letter:$bad")
        }.defnTerm

        val defaultTerm = {
          val default = extract[Term.Arg](params)
          if (isMandatory && default.nonEmpty)
            abort(pos, s"Mandatory arg ${argName.syntax} cannot have default value.")
          //default for Boolean is false.
          if (default.isEmpty && tpe.syntax == "Boolean") q"Option(false)"
          else default.defnTerm
        }
        val name = Lit.String(argName.syntax)
        defName match {
          case q"paramDef" =>
            val term =
              q"""runtime.buildParameter[$tpe]($TERM_NAME = $name,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatoryTerm,
                                               argValue = ${singleValue(tpe, defaultTerm)})"""
            TermParam(name.value, term, pos, tpe, topCmdSymbol)
          case q"paramDefVariable" =>
            val term =
              q"""runtime.buildParameter[$tpe]($TERM_NAME = $name,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatoryTerm,
                                               argValue = ${variableValue(tpe, defaultTerm)})"""
            TermParam(name.value, term, pos, tpe, topCmdSymbol)
          case q"optDef" =>
            val term =
              q"""runtime.buildOptionArg[$tpe]($TERM_NAME = $name,
                                               $TERM_ABBREVIATION = $abbr,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatoryTerm,
                                               argValue = ${singleValue(tpe, defaultTerm)})"""
            TermOpt(name.value, term, pos, tpe, topCmdSymbol)
          case q"optDefVariable" =>
            val term =
              q"""runtime.buildOptionArg[$tpe]($TERM_NAME = $name,
                                               $TERM_ABBREVIATION = $abbr,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatoryTerm,
                                               argValue = ${variableValue(tpe, defaultTerm)})"""
            TermOpt(name.value, term, pos, tpe, topCmdSymbol)
          case q"propDef" =>
            val argValueType = Type.Name(s"(String,${tpe.syntax})")
            val flagTerm = {
              val flag = extract[String](params)
              flag match {
                case Some(f) =>
                  if (!f.matches("""[A-Z]{1}"""))
                    abort(pos, s"Flag of prop ${argName.syntax}  must be of one upper case letter.")
                  f.defnTerm
                case None =>
                  if (!name.value.matches("""[A-Z]{1}"""))
                    abort(pos, s"If flag not specified," +
                      s" prop ${argName.syntax} must have one upper case letter as its name.")
                  name
              }
            }
            val term =
              q"""runtime
                  .buildPropertyArg[$tpe]($TERM_NAME = $name,
                                          flag = $flagTerm,
                                          $TERM_DESCRIPTION = $description,
                                          variableValue = ${variableValue(argValueType, defaultTerm)})"""
            TermProp(name.value, flagTerm.syntax, term, pos, tpe)
        }
      case stat@q"val $argName: $_ = $defName(..$params)"
        if defName.syntax.contains("Def") =>
        abort(s"No type found for argument def: $stat")
    }
  }
  //todo: add name conflict check.

  private def singleValue(tpe: Type, default: Term): Term =
    q"""runtime.buildSingleValue[$tpe]($default)"""
  private def variableValue(tpe: Type, default: Term): Term =
    q"""runtime.buildVariableValue[$tpe]($default.toSeq.flatten)"""
}

private sealed trait TermValueArg extends TermArg
private final case class TermCmd(name: String, term: Term, pos: Position) extends TermArg {
  val tpe: Type.Name = TYPE_NOTHING
}
private final case class TermParam(name: String, term: Term, pos: Position, tpe: Type,
                                   parent: Lit.Symbol) extends TermValueArg
private final case class TermOpt(name: String, term: Term, pos: Position, tpe: Type,
                                 parent: Lit.Symbol) extends TermValueArg
private final case class TermProp(name: String, flag: String,
                                  term: Term, pos: Position, tpe: Type) extends TermArg
private final case class TermPrior(name: String, term: Term, pos: Position,
                                   parent: Lit.Symbol) extends TermArg {
  val tpe: Type.Name = TYPE_NOTHING
}
private final
case class TermCommandEntry(term: Term, children: immutable.Seq[TermCmdNode])

private object TermCmd {
  val dummy: TermCmd = TermCmd("", q"_", Position.None)
}

private object TermParam {
  implicit val definable: Definable[TermParam] = (a: TermParam) => {
    q"""runtime.buildParamNode[${a.tpe}](
            entity = ${a.term},
            value = Nil,
            parent = ${a.parent}
        )"""
  }
}

private object TermOpt {
  implicit val definable: Definable[TermOpt] = (a: TermOpt) => {
    q"""runtime.buildOptNode[${a.tpe}](
            entity = ${a.term},
            value = Nil,
            parent = ${a.parent}
        )"""
  }
}

private object TermProp {
  implicit val definable: Definable[TermProp] = (a: TermProp) => {
    q"""runtime.buildPropNode[${a.tpe}](
            entity = ${a.term},
            value = Nil
        )"""
  }
}

private object TermPrior {
  implicit val definable: Definable[TermPrior] = (a: TermPrior) => {
    q"""runtime.buildPriorNode(
            entity = ${a.term},
            parent = ${a.parent}
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

  def getTerm(isMandatory: Boolean): Term = q"runtime.buildCmdEntry(${Lit.Boolean(isMandatory)})"

  /** Optional cmd entry with zero children. */
  val placeHolder: TermCommandEntry = {
    val term = q"runtime.buildCmdEntry(false)"
    TermCommandEntry(term = term, children = immutable.Seq.empty)
  }

  /** If cmdNodes are empty, return a placeHolder, otherwise return a mandatory entry. */
  def createWithCmdNodes(commandNodes: immutable.Seq[TermCmdNode]): TermCommandEntry = {
    if (commandNodes.isEmpty) this.placeHolder
    else
      TermCommandEntry(term = q"runtime.buildCmdEntry(true)", children = commandNodes)
  }
}