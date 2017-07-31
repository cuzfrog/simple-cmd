package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.macros.Constants._

import scala.collection.immutable
import scala.meta._

private final
case class TermCmdNode(cmd: TermCmd,
                       params: immutable.Seq[TermParam],
                       opts: immutable.Seq[TermOpt],
                       parent: Option[TermCmdNode],
                       subCmdEntry: TermCommandEntry,
                       limitations: immutable.Seq[(Limitation, immutable.Seq[String])] = Nil)

private final
case class TermArgTree(topParams: immutable.Seq[TermParam],
                       topOpts: immutable.Seq[TermOpt],
                       cmdEntry: TermCommandEntry,
                       topLimitations: immutable.Seq[(Limitation, immutable.Seq[String])] = Nil,
                       globalLimitations: immutable.Seq[(Limitation, immutable.Seq[String])] = Nil)

private object TermTree {
  def collectTreeDefDsl(stats: immutable.Seq[Stat]): immutable.Seq[Term.Arg] = {
    stats.collect {
      case q"argTreeDef(..$params)" => params
    }.flatten
  }
}

/**
  * Mutual relationship among value arguments. Defined in tree DSL.
  *
  * @see [[com.github.cuzfrog.scmd.ScmdTreeDefDSL]]
  */
private sealed trait Limitation
private object Limitation {
  def fromOperator(operator: String): Limitation = operator match {
    case "|" => MutuallyExclusive
    case "&" => MutuallyDependent
    case bad => throw new IllegalArgumentException(s"Illegal limitation operator:$bad")
  }
  case object MutuallyExclusive extends Limitation
  case object MutuallyDependent extends Limitation
}

private object TermCmdNode {
  implicit val definable: Definable[TermCmdNode] = (a: TermCmdNode) => recDefine(a)

  private def recDefine(a: TermCmdNode): Term = {
    val entity = q"entity = ${a.cmd.term}"
    val params = q"params = $TERM_immutable.Seq(..${a.params.map(_.defnTerm)})"
    val opts = q"opts = $TERM_immutable.Seq(..${a.opts.map(_.defnTerm)})"
    val parent = a.parent match {
      case Some(p) => q"parent = Option(${p.defnTerm})"
      case None => q"parent = None"
    }
    val subCmdEntry = q"subCmdEntry = ${a.subCmdEntry.defnTerm}"

    q"""runtime.buildCmdNode(
          $entity,
          $params,
          $opts,
          $parent,
          $subCmdEntry
        )"""
  }
}

private object TermArgTree {
  implicit val definable: Definable[TermArgTree] = (a: TermArgTree) => {
    val topParams = a.topParams.map(_.defnTerm)
    val topOpts = a.topOpts.map(_.defnTerm)
    val cmdEntry = a.cmdEntry.defnTerm
    q"""runtime.buildArgTree(
          topParams = $TERM_immutable.Seq(..$topParams),
          topOpts = $TERM_immutable.Seq(..$topOpts),
          cmdEntry = $cmdEntry
        )"""
  }
}