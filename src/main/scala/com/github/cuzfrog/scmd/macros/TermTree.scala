package com.github.cuzfrog.scmd.macros

import scala.annotation.tailrec
import scala.meta._
import scala.collection.immutable

import Constants._

private final case class TermCmdNode(cmd: TermCmd,
                                     params: immutable.Seq[TermParam],
                                     opts: immutable.Seq[TermOpt],
                                     children: immutable.Seq[TermCmdNode])

private final case class TermArgTree(commands: immutable.Seq[TermCmdNode],
                                      topParams: immutable.Seq[TermParam],
                                      topOpts: immutable.Seq[TermOpt])

private object TermCmdNode {
  implicit val definable: Definable[TermCmdNode] = (a: TermCmdNode) => recDefine(a)

  private def recDefine(a: TermCmdNode): Term = {
    val entityVal = q"val entity = ${a.cmd.arg}"
    val paramsVal = q"val params = $TERM_immutable.Seq(..${a.params.map(_.defnRuntimeTerm)})"
    val optsVal = q"val opts = $TERM_immutable.Seq(..${a.opts.map(_.defnRuntimeTerm)})"
    val childrenVal = a.children match {
      case Nil => q"val children = $TERM_immutable.Seq.empty[CmdNode]"
      case cdren => q"val children = $TERM_immutable.Seq(..${cdren.map(recDefine)})"
    }

    q"""new com.github.cuzfrog.scmd.CmdNode{
          $entityVal
          $paramsVal
          $optsVal
          $childrenVal
        }"""
  }
}

private object TermArgTree {
  implicit val definable: Definable[TermArgTree] = (a: TermArgTree) => {
    val topParams = a.topParams.map(_.defnRuntimeTerm)
    val topOpts = a.topOpts.map(_.defnRuntimeTerm)
    val commands = a.commands.map(_.defnRuntimeTerm)
    q"""com.github.cuzfrog.scmd.ArgTree(
          commands = $TERM_immutable.Seq(..$commands),
          topParams = $TERM_immutable.Seq(..$topParams),
          topOpts = $TERM_immutable.Seq(..$topOpts)
        )"""
  }
}