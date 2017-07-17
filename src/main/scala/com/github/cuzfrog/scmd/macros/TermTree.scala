package com.github.cuzfrog.scmd.macros

import scala.meta._
import scala.collection.immutable

import Constants._

private final case class TermCmdNode(cmd: TermCmd,
                                     params: immutable.Seq[TermParam],
                                     opts: immutable.Seq[TermOpt],
                                     subCmdEntry: TermCommandEntry)

private final case class TermArgTree(topParams: immutable.Seq[TermParam],
                                     topOpts: immutable.Seq[TermOpt],
                                     cmdEntry: TermCommandEntry)

private object TermCmdNode {
  implicit val definable: Definable[TermCmdNode] = (a: TermCmdNode) => recDefine(a)

  private def recDefine(a: TermCmdNode): Term = {
    val entity = q"val entity = ${a.cmd.term}"
    val params = q"val params = $TERM_immutable.Seq(..${a.params.map(_.defnTerm)})"
    val opts = q"val opts = $TERM_immutable.Seq(..${a.opts.map(_.defnTerm)})"
    val subCmdEntry = q"val subCmdEntry = ${a.subCmdEntry.defnTerm}"

    q"""new com.github.cuzfrog.scmd.CmdNode{
          $entity
          $params
          $opts
          $subCmdEntry
        }"""
  }
}

private object TermArgTree {
  implicit val definable: Definable[TermArgTree] = (a: TermArgTree) => {
    val topParams = a.topParams.map(_.defnTerm)
    val topOpts = a.topOpts.map(_.defnTerm)
    val cmdEntry = a.cmdEntry.defnTerm
    q"""com.github.cuzfrog.scmd.ArgTree(
          topParams = $TERM_immutable.Seq(..$topParams),
          topOpts = $TERM_immutable.Seq(..$topOpts),
          cmdEntry = $cmdEntry
        )"""
  }
}