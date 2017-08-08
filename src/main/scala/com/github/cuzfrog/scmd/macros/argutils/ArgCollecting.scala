package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.Defaults
import com.github.cuzfrog.scmd.internal.RawArgMacro.extract
import com.github.cuzfrog.scmd.internal.SimpleLogging
import com.github.cuzfrog.scmd.macros.Constants.{TERM_ABBREVIATION, TERM_DESCRIPTION, TERM_IS_MANDATORY, TERM_NAME}
import com.github.cuzfrog.scmd.macros.TermArg.{debug, singleValue, variableValue}
import com.github.cuzfrog.scmd.macros.{TermArg, TermCmd, TermOpt, TermParam, TermPrior, TermProp}

import scala.collection.immutable
import scala.meta._

/** Collected from statements, not fully type safe. */
private trait RawArg
private object RawArg {
  case class RawCommand(name: String, description: Option[String], pos: Position)
  case class RawPrior(name: String, description: Option[String],
                      alias: Option[Term], pos: Position)
  case class RawParam(name: String, description: Option[String], tpe: Type,
                      isMandatory: Boolean, argValue: Term, pos: Position)
  case class RawOpt(name: String, abbr: Option[String], description: Option[String], tpe: Type,
                    isMandatory: Boolean, argValue: Term, pos: Position)
  case class RawProps(name: String, flag: String, description: Option[String], tpe: Type,
                      variableValue: Term, pos: Position)
}

private object ArgCollectImpl extends SimpleLogging {

  import ArgUtils.{Selection, extractSelection}
  import RawArg._
  import com.github.cuzfrog.scmd.macros.DefineTermOps

  def collectTermArg(stats: immutable.Seq[Stat]): immutable.Seq[RawArg] = {


    (stats zip stats.map(extractSelection)) collect {
      case (q"val $argName: $_ = $defName(..$params)", _) if defName.isInstanceOf[Term.Name] =>
        implicit val pos: Position = argName.pos
        val name = argName.syntax
        val description = extract[String](params)
        defName match {
          case q"cmdDef" =>
            RawCommand(name, description, pos)
          case q"priorDef" =>
            val alias = extract[Term](params)
            val matchName = extract[Boolean](params).getOrElse(Defaults.priorMatchName)
            if (alias.isEmpty && !matchName)
              abort(pos, s"PriorArg $name can never be matched," +
                s" no alias defined and not going to match name.")
            RawPrior(name, description, alias, pos)
        }
      case (q"val $argName: $_ = $defName[$tpe](..$params)", selections) =>
        debug(s"Collected $defName: $argName[$tpe]")
        implicit val pos: Position = argName.pos
        val name = argName.syntax
        val description = extract[String](params)
        val isMandatory: Boolean = {
          val isM = selections.contains(Selection.Mandatory)
          if (isM && tpe.syntax == "Boolean")
            abort(pos, s"Boolean arg: $name cannot be mandatory.")
          isM
        }
        val abbr = extract[String](params).map {
          case ab if ab.matches("""\w+""") => ab
          case bad => abort(pos, s"Opt abbreviation can only contain letter:$bad")
        }

        val defaultTerm = {
          val default = selections.collectFirst { case Selection.WithDefault(v) => v }
          if (isMandatory && default.nonEmpty)
            abort(pos, s"Mandatory arg ${argName.syntax} cannot have default value.")
          //default for Boolean is false.
          if (default.isEmpty && tpe.syntax == "Boolean") q"Option(false)"
          else default.defnTerm
        }

        defName match {
          case q"paramDef" =>
            RawParam(name, description, tpe, isMandatory, singleValue(tpe, defaultTerm), pos)
          case q"paramDefVariable" =>
            RawParam(name, description, tpe, isMandatory, variableValue(tpe, defaultTerm), pos)
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

  private def singleValue(tpe: Type, default: Term): Term =
    q"""runtime.buildSingleValue[$tpe]($default)"""
  private def variableValue(tpe: Type, default: Term): Term =
    q"""runtime.buildVariableValue[$tpe]($default.toSeq.flatten)"""

}
