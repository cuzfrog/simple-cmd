package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.internal.SimpleLogging
import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.macros.argutils.RawArg
import com.github.cuzfrog.scmd.{AppInfo, Argument, BuiltInArg, Command}

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
  def toTermArg(rawArg: RawArg)(implicit appInfo: AppInfo): TermArg = {
    import RawArg._

    val topCmdSymbol = Lit.Symbol(Command.topCmd(appInfo.name).symbol)

    rawArg match {
      case r: RawCommand =>
        val term =
          q"""runtime.buildCommand($TERM_NAME = ${r.name.defnTerm},
                                   $TERM_DESCRIPTION = ${r.description.defnTerm})"""
        TermCmd(r.name, term, r.pos)
      case r: RawPrior =>
        val term =
          q"""runtime.buildPriorArg($TERM_NAME = ${r.name.defnTerm},
                                    alias = ${r.alias.defnTerm},
                                    $TERM_DESCRIPTION = ${r.description.defnTerm},
                                    matchName = ${r.matchName.defnTerm})"""
        TermPrior(r.name, term, r.pos, topCmdSymbol)


      case r: RawParam =>
        val term =
          q"""runtime.buildParameter[${r.tpe}]($TERM_NAME = ${r.name.defnTerm},
                                               $TERM_DESCRIPTION = ${r.description.defnTerm},
                                               $TERM_IS_MANDATORY = ${r.isMandatory.defnTerm},
                                               argValue = ${r.argValue})"""
        TermParam(r.name, term, r.pos, r.tpe, topCmdSymbol, r.isMandatory, r.isVariable)

      case r: RawOpt =>
        val term =
          q"""runtime.buildOptionArg[${r.tpe}]($TERM_NAME = ${r.name.defnTerm},
                                               $TERM_ABBREVIATION = ${r.abbr.defnTerm},
                                               $TERM_DESCRIPTION = ${r.description.defnTerm},
                                               $TERM_IS_MANDATORY = ${r.isMandatory.defnTerm},
                                               argValue = ${r.argValue})"""
        TermOpt(r.name, term, r.pos, r.tpe, topCmdSymbol)

      case r: RawProp =>
        val term =
          q"""runtime
                  .buildPropertyArg[${r.tpe}]($TERM_NAME = ${r.name.defnTerm},
                                          flag = ${r.flag.defnTerm},
                                          $TERM_DESCRIPTION = ${r.description.defnTerm},
                                          variableValue = ${r.variableValue})"""
        TermProp(r.name, r.flag, term, r.pos, r.tpe)
    }
  }

  //todo: (low priority) make builtInArgs more generic.
  /** Used in ScmdDefMacro to generate built-in args. */
  def builtInArgs(implicit appInfo: AppInfo): immutable.Seq[TermArg] = Argument.builtInArgs.map {
    case (symbol, _) =>
      val topCmdSymbol = Lit.Symbol(Command.topCmd(appInfo.name).symbol)
      new TermPrior(symbol.name,
        q"runtime.builtInArgs(${Lit.Symbol(symbol)})", Position.None, topCmdSymbol) with BuiltInArg
  }.to[immutable.Seq]
}

private sealed trait TermValueArg extends TermArg
private final case class TermCmd(name: String, term: Term, pos: Position) extends TermArg {
  val tpe: Type.Name = TYPE_NOTHING
}
private sealed case class TermParam(name: String, term: Term, pos: Position, tpe: Type,
                                    parent: Lit.Symbol,
                                    isMandatory: Boolean, isVariable: Boolean) extends TermValueArg
private sealed case class TermOpt(name: String, term: Term, pos: Position, tpe: Type,
                                  parent: Lit.Symbol) extends TermValueArg
private sealed case class TermProp(name: String, flag: String,
                                   term: Term, pos: Position, tpe: Type) extends TermArg
private sealed case class TermPrior(name: String, term: Term, pos: Position,
                                    parent: Lit.Symbol) extends TermArg {
  val tpe: Type.Name = TYPE_NOTHING
}
private final
case class TermCommandEntry(term: Term, children: immutable.Seq[TermCmdNode])

private object TermCmd {
  val dummy: TermCmd = TermCmd("", q"_", Position.None)
}

private object TermParam {
  implicit val definable: Definable[TermParam] =
    new Definable[TermParam] {
      override def defnTerm(a: TermParam): Term = {
        q"""runtime.buildParamNode[${a.tpe}](
            entity = ${a.term},
            value = Nil,
            parent = ${a.parent}
        )"""
      }
    }
}

private object TermOpt {
  implicit val definable: Definable[TermOpt] =
    new Definable[TermOpt]{
      override def defnTerm(a: TermOpt): Term = {
        q"""runtime.buildOptNode[${a.tpe}](
            entity = ${a.term},
            value = Nil,
            parent = ${a.parent}
        )"""
      }
    }
}

private object TermProp {
  implicit val definable: Definable[TermProp] =
    new Definable[TermProp]{
      override def defnTerm(a: TermProp): Term = {
        q"""runtime.buildPropNode[${a.tpe}](
            entity = ${a.term},
            value = Nil
        )"""
      }
    }
}

private object TermPrior {
  implicit val definable: Definable[TermPrior] =
    new Definable[TermPrior]{
      override def defnTerm(a: TermPrior): Term = {
        q"""runtime.buildPriorNode(
            entity = ${a.term},
            parent = ${a.parent}
        )"""
      }
    }
}

private object TermCommandEntry {
  implicit val definable: Definable[TermCommandEntry] =
    new Definable[TermCommandEntry]{
      override def defnTerm(a: TermCommandEntry): Term = {
        val children = a.children match {
          case Nil => q"$TERM_immutable.Seq.empty[Int]"
          case cdren => q"$TERM_immutable.Seq(..${cdren.map(_.defnTerm)})"
        }
        q"""runtime.buildCmdEntryNode(
          entity = ${a.term},
          children = $children
        )"""
      }
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