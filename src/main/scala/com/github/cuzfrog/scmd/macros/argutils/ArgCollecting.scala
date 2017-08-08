package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.Defaults
import com.github.cuzfrog.scmd.internal.RawArgMacro.extract
import com.github.cuzfrog.scmd.internal.SimpleLogging

import scala.collection.immutable
import scala.meta._

/** Collected from statements, not fully type safe. */
private[macros] trait RawArg
private[macros] object RawArg {
  case class RawCommand(name: String, description: Option[String], pos: Position) extends RawArg
  case class RawPrior(name: String, description: Option[String], matchName: Boolean,
                      alias: Option[Term], pos: Position) extends RawArg
  case class RawParam(name: String, description: Option[String], tpe: Type, isMandatory: Boolean,
                      hasDefault: Boolean, argValue: Term, pos: Position) extends RawArg
  case class RawOpt(name: String, abbr: Option[String], description: Option[String], tpe: Type,
                    isMandatory: Boolean, hasDefault: Boolean,
                    argValue: Term, pos: Position) extends RawArg
  case class RawProp(name: String, flag: String, description: Option[String], tpe: Type,
                     hasDefault: Boolean, variableValue: Term, pos: Position) extends RawArg
}

private object ArgCollectImpl extends SimpleLogging {

  import RawArg._
  import com.github.cuzfrog.scmd.macros.DefineTermOps

  def collectRawArg(stats: immutable.Seq[Stat]): immutable.Seq[RawArg] = {

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
            RawPrior(name, description, matchName, alias, pos)
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
        val default = selections.collectFirst { case Selection.WithDefault(v) => v }
        val hasDefault = default.nonEmpty
        val defaultTerm = {
          if (isMandatory && hasDefault)
            abort(pos, s"Mandatory arg ${argName.syntax} cannot have default value.")
          //default for Boolean is false.
          if (default.isEmpty && tpe.syntax == "Boolean") q"Option(false)"
          else default.defnTerm
        }
        defName match {
          case q"paramDef" =>
            RawParam(name, description, tpe, isMandatory, hasDefault,
              singleValue(tpe, defaultTerm), pos)
          case q"paramDefVariable" =>
            RawParam(name, description, tpe, isMandatory, hasDefault,
              variableValue(tpe, defaultTerm), pos)
          case q"optDef" =>
            RawOpt(name, abbr, description, tpe, isMandatory, hasDefault,
              singleValue(tpe, defaultTerm), pos)
          case q"optDefVariable" =>
            RawOpt(name, abbr, description, tpe, isMandatory, hasDefault,
              variableValue(tpe, defaultTerm), pos)
          case q"propDef" =>
            val argValueType = Type.Name(s"(String,${tpe.syntax})")
            val flag = extract[String](params) match {
              case Some(f) =>
                if (!f.matches("""[A-Z]{1}"""))
                  abort(pos, s"Flag of prop ${argName.syntax}  must be of one upper case letter.")
                f
              case None =>
                if (!name.matches("""[A-Z]{1}"""))
                  abort(pos, s"If flag not specified," +
                    s" prop ${argName.syntax} must have one upper case letter as its name.")
                name
            }
            RawProp(name, flag, description, tpe, hasDefault,
              variableValue(argValueType, defaultTerm), pos)
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

  private def extractSelection(stat: Stat): Seq[Selection] = {
    val selections = stat match {
      case q"val $argName: $_ = $defName(..$params).$s1" => Seq(s1)
      case q"val $argName: $_ = $defName(..$params).$s1.$s2" => Seq(s1, s2)
      case _ => Nil
    }
    selections.map {
      case q"mandatory" => Selection.Mandatory
      case q"withDefault($param)" =>
        val value = param match {
          case Term.Arg.Named(_, v) => v
          case v => v
        }
        Selection.WithDefault(value)
    }
  }

  sealed trait Selection
  object Selection {
    case object Mandatory extends Selection
    case class WithDefault(v: Term.Arg) extends Selection
  }

}
