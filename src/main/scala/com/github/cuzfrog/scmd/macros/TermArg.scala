package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.{Command, Defaults}
import com.github.cuzfrog.scmd.internal.{RawArgMacro, SimpleLogging}
import com.github.cuzfrog.scmd.macros.Constants._

import scala.collection.immutable
import scala.meta._

//todo: add positions.

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
  override protected val loggerLevel: SimpleLogging.Level = SimpleLogging.Info
  def collectTermArg(stats: immutable.Seq[Stat]): immutable.Seq[TermArg] = {
    import RawArgMacro.extract

    stats collect {
      case q"val $cmd: $_ = cmdDef(..$params)" =>
        implicit val pos: Position = cmd.pos
        val description = extract[String](params).defnTerm
        val term =
          q"""runtime.buildCommand($TERM_NAME = ${Lit.String(cmd.syntax)},
                                   $TERM_DESCRIPTION = $description)"""
        TermCmd(cmd.syntax, term, pos)
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
          default.defnTerm
        }
        val name = Lit.String(argName.syntax)
        defName match {
          case q"paramDef" =>
            val term =
              q"""runtime.buildParameter[$tpe]($TERM_NAME = $name,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatoryTerm,
                                               argValue = ${singleValue(tpe, defaultTerm)})"""
            TermParam(name.value, term, pos, tpe)
          case q"paramDefVariable" =>
            val term =
              q"""runtime.buildParameter[$tpe]($TERM_NAME = $name,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatoryTerm,
                                               argValue = ${variableValue(tpe, defaultTerm)})"""
            TermParam(name.value, term, pos, tpe)
          case q"optDef" =>
            val term =
              q"""runtime.buildOptionArg[$tpe]($TERM_NAME = $name,
                                               $TERM_ABBREVIATION = $abbr,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatoryTerm,
                                               argValue = ${singleValue(tpe, defaultTerm)})"""
            TermOpt(name.value, term, pos, tpe)
          case q"optDefMultiple" =>
            val term =
              q"""runtime.buildOptionArg[$tpe]($TERM_NAME = $name,
                                               $TERM_ABBREVIATION = $abbr,
                                               $TERM_DESCRIPTION = $description,
                                               $TERM_IS_MANDATORY = $isMandatoryTerm,
                                               argValue = ${variableValue(tpe, defaultTerm)})"""
            TermOpt(name.value, term, pos, tpe)
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
private final
case class TermCmd(name: String, term: Term, pos: Position) extends TermArg {
  val tpe: Type.Name = TYPE_NOTHING
}
private final
case class TermParam(name: String, term: Term, pos: Position, tpe: Type,
                     parent: Option[Lit.Symbol] = None) extends TermValueArg {
  def withParent(symbol: scala.Symbol = Command.topCmd.symbol): TermParam =
    this.copy(parent = Some(Lit.Symbol(symbol)))
}
private final
case class TermOpt(name: String, term: Term, pos: Position, tpe: Type,
                   parent: Option[Lit.Symbol] = None) extends TermValueArg {
  def withParent(symbol: scala.Symbol = Command.topCmd.symbol): TermOpt =
    this.copy(parent = Some(Lit.Symbol(symbol)))
}
private final
case class TermProp(name: String, flag: String,
                    term: Term, pos: Position, tpe: Type) extends TermArg
private final
case class TermCommandEntry(term: Term, children: immutable.Seq[TermCmdNode])

private object TermCmd {
  val dummy: TermCmd = TermCmd("", q"_", Position.None)
}

private object TermParam {
  implicit val definable: Definable[TermParam] = (a: TermParam) => {
    val parent =
      a.parent.getOrElse(throw new AssertionError(s"Parent empty of TermParam: ${a.name}"))
    q"""runtime.buildParamNode[${a.tpe}](
            entity = ${a.term},
            value = Nil,
            parent = $parent
        )"""
  }
}

private object TermOpt {
  implicit val definable: Definable[TermOpt] = (a: TermOpt) => {
    val parent =
      a.parent.getOrElse(throw new AssertionError(s"Parent empty of TermOpt: ${a.name}"))
    q"""runtime.buildOptNode[${a.tpe}](
            entity = ${a.term},
            value = Nil,
            parent = $parent
        )"""
  }
}

private object TermProp{
  implicit val definable: Definable[TermProp] = (a: TermProp) => {
    q"""runtime.buildPropNode[${a.tpe}](
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
      TermCommandEntry(term = q"runtime.buildCmdEntry(true)", children = immutable.Seq.empty)
  }
}