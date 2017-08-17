package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.{Defaults, OptionArg}
import com.github.cuzfrog.scmd.internal.RawArgMacro.extract
import com.github.cuzfrog.scmd.internal.SimpleLogging

import scala.annotation.tailrec
import scala.collection.immutable
import scala.meta._

/** Collected from statements, not fully type safe. */
private[macros] sealed trait RawArg {
  def name: String
  def pos: Position
}
private[macros] sealed trait RawTypedArg extends RawArg {
  def tpe: Type
  def composedTpe: Type
}
private[macros] object RawArg {
  case class RawCommand(name: String, description: Option[String], pos: Position) extends RawArg
  case class RawPrior(name: String, description: Option[String], matchName: Boolean,
                      alias: immutable.Seq[String], pos: Position) extends RawArg
  case class RawParam(name: String, description: Option[String], tpe: Type, isMandatory: Boolean,
                      hasDefault: Boolean, isVariable: Boolean,
                      argValue: Term, pos: Position, composedTpe: Type) extends RawTypedArg
  case class RawOpt(name: String, abbr: Option[String], description: Option[String], tpe: Type,
                    isMandatory: Boolean, hasDefault: Boolean, isVariable: Boolean,
                    argValue: Term, pos: Position, composedTpe: Type) extends RawTypedArg
  case class RawProp(name: String, flag: String, description: Option[String], tpe: Type,
                     hasDefault: Boolean, variableValue: Term, pos: Position,
                     composedTpe: Type) extends RawTypedArg
}

private object ArgCollectImpl extends SimpleLogging {
  override protected implicit val loggerLevel: SimpleLogging.Level = SimpleLogging.Info

  import RawArg._
  import com.github.cuzfrog.scmd.macros.DefineTermOps
  import ArgUtils.getComposedTpe

  def collectRawArg(stats: immutable.Seq[Stat]): immutable.Seq[RawArg] = {

    val collected: immutable.Seq[RawArg] = stats.map(extractSelection) collect {
      case (q"val $argName: $_ = $defName(..$params)", _) if defName.isInstanceOf[Term.Name] =>
        debug(s"Collected $defName: $argName.")
        implicit val pos: Position = argName.pos
        val name = argName.syntax
        val description = extract[String](params)
        defName match {
          case q"cmdDef" =>
            RawCommand(name, description, pos)
          case q"priorDef" =>
            val alias = extract[Term](params).to[immutable.Seq].flatMap {
              case q"$seqName(..$names)" =>
                if (!seqName.syntax.endsWith("Seq") && !seqName.syntax.endsWith("List"))
                  abort(pos, s"please pass 'Seq' or 'List' of alias instead of ${seqName.syntax}")
                names.map {
                  case Lit.String(aliasName) => aliasName
                  case bad => abort(pos, s"please pass in literal String of alias for: ${bad.syntax}")
                }
              case bad =>
                abort(pos, s"""please pass Seq("alias1","alias2") instead of ${bad.syntax}""")
            }
            val matchName = extract[Boolean](params).getOrElse(Defaults.priorMatchName)
            if (alias.isEmpty && !matchName)
              abort(pos, s"PriorArg $name can never be matched," +
                s" no alias defined and not going to match name.")
            RawPrior(name, description, matchName, alias, pos)
        }
      case (q"val $argName: $_ = $defName[$tpe](..$params)", selections) =>
        debug(s"Collected $defName: $argName[$tpe] with selections:$selections")
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
            val composedTpe =
              getComposedTpe(isMandatory, hasDefault, Types.parameter, tpe, Types.singleValue)
            RawParam(name, description, tpe, isMandatory, hasDefault, isVariable = false,
              singleValue(tpe, defaultTerm), pos, composedTpe)
          case q"paramDefVariable" =>
            val composedTpe =
              getComposedTpe(isMandatory, hasDefault, Types.parameter, tpe, Types.variableValue)
            RawParam(name, description, tpe, isMandatory, hasDefault, isVariable = true,
              variableValue(tpe, defaultTerm), pos, composedTpe)
          case q"optDef" =>
            val composedTpe =
              getComposedTpe(isMandatory, hasDefault, Types.optionArg, tpe, Types.singleValue)
            RawOpt(name, abbr, description, tpe, isMandatory, hasDefault, isVariable = false,
              singleValue(tpe, defaultTerm), pos, composedTpe)
          case q"optDefVariable" =>
            val composedTpe =
              getComposedTpe(isMandatory, hasDefault, Types.optionArg, tpe, Types.variableValue)
            RawOpt(name, abbr, description, tpe, isMandatory, hasDefault, isVariable = true,
              variableValue(tpe, defaultTerm), pos, composedTpe)
          case q"propDef" =>
            val composedTpe =
              getComposedTpe(isMandatory, hasDefault, Types.propertyArg, tpe, Types.variableValue)
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
              variableValue(argValueType, defaultTerm), pos, composedTpe)
        }
      case (stat@q"val $argName: $_ = $defName(..$params)", _) if defName.syntax.contains("Def") =>
        abort(s"No type found for argument def: $stat")
    }
    val conflicts = getNameConflict(collected)
    if (conflicts.nonEmpty)
      abort(conflicts.head.pos, s"Name conflict found for args: ${conflicts.map(_.name).mkString(",")}")
    collected
  }

  private def singleValue(tpe: Type, default: Term): Term =
    q"""runtime.buildSingleValue[$tpe]($default)"""
  private def variableValue(tpe: Type, default: Term): Term =
    q"""runtime.buildVariableValue[$tpe]($default.toSeq.flatten)"""

  private def extractSelection(stat: Stat): (Stat, Seq[Selection]) = {
    val pos: Position = stat.pos
    @tailrec
    def recSelect(rhs: Term, acc: Seq[Selection]): (Term, Seq[Selection]) = rhs match {
      case Term.Select(inner, selectName) =>
        val s = Selection.fromTerm(selectName).getOrElse(
          abort(pos, s"Calling ${selectName.value} in arg defs '${stat.syntax}' is not supported."))
        recSelect(inner, acc :+ s)
      case Term.Apply(Term.Select(inner, selectName), params) =>
        val s = Selection.fromTerm(selectName, params).getOrElse(
          abort(pos, s"Calling ${selectName.value} in arg defs '${stat.syntax}' is not supported."))
        recSelect(inner, acc :+ s)
      case method@Term.Apply(_: Term.Name, _) => method -> acc
      case method@Term.Apply(_: Term.ApplyType, _) => method -> acc
      case other => other -> Nil
    }

    stat match {
      case valDef: Defn.Val =>
        val (defM, selections) = recSelect(valDef.rhs, Seq())
        valDef.copy(rhs = defM) -> selections
      case other =>
        other -> Nil
    }
  }
  /** Return rawArgs that have conflicting names. */
  private def getNameConflict(rawArgs: immutable.Seq[RawArg]): immutable.Seq[RawArg] = {
    val rawPairs = rawArgs.map {
      case raw: RawCommand => raw -> List(raw.name)
      case raw: RawPrior => raw -> ((if (raw.matchName) List(raw.name) else Nil) ++ raw.alias)
      case raw: RawParam => raw -> List(raw.name)
      case raw: RawOpt => raw ->
        (List("--" + raw.name, "--" + OptionArg.camelCase2hyphen(raw.name)).distinct
          ++ raw.abbr.map("-" + _))
      case raw: RawProp => raw -> List("-" + raw.flag)
    }
    val allNames = rawPairs.flatMap(_._2)
    val duplicates =
      allNames.groupBy(identity).collect { case (k, v) if v.lengthCompare(1) > 0 => k }.toList
    rawPairs.collect { case (r, names) if duplicates.intersect(names).nonEmpty => r }
  }

  sealed trait Selection
  object Selection {
    def fromTerm(selectName: Term): Option[Mandatory.type] = selectName match {
      case q"mandatory" => Some(Mandatory)
      case _ => None
    }

    def fromTerm(selectName: Term,
                 params: immutable.Seq[Term.Arg]): Option[WithDefault] = selectName match {
      case q"withDefault" =>
        val value = params.head match {
          case Term.Arg.Named(_, v) => v
          case v => v
        }
        Some(WithDefault(value))
      case _ => None
    }

    case object Mandatory extends Selection
    case class WithDefault(v: Term.Arg) extends Selection
  }

}
