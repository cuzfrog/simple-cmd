package com.github.cuzfrog.scmd.macros

import scala.meta._
import scala.collection.immutable

import Constants._

private final case class TermCmdNode(cmd: TermCmd,
                                     params: immutable.Seq[TermParam],
                                     opts: immutable.Seq[TermOpt],
                                     parent: Option[TermCmdNode],
                                     subCmdEntry: TermCommandEntry)

private final case class TermArgTree(topParams: immutable.Seq[TermParam],
                                     topOpts: immutable.Seq[TermOpt],
                                     cmdEntry: TermCommandEntry)

private object TermCmdNode {
  implicit val definable: Definable[TermCmdNode] = (a: TermCmdNode) => recDefine(a)

  private def recDefine(a: TermCmdNode): Term = {
    val entity = q"entity = ${a.cmd.term}"
    val params = q"params = $TERM_immutable.Seq(..${a.params.map(_.defnTerm)})"
    val opts = q"opts = $TERM_immutable.Seq(..${a.opts.map(_.defnTerm)})"
    val parent = a.parent match{
      case Some(p) => q"parent = Option(${p.defnTerm})"
      case None => q"parent = None"
    }
    val subCmdEntry = q"subCmdEntry = ${a.subCmdEntry.defnTerm}"

    q"""scmdRuntime.buildCmdNode(
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
    q"""scmdRuntime.buildArgTree(
          topParams = $TERM_immutable.Seq(..$topParams),
          topOpts = $TERM_immutable.Seq(..$topOpts),
          cmdEntry = $cmdEntry
        )"""
  }
}