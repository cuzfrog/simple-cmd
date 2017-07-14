package com.github.cuzfrog.scmd.macros

import scala.annotation.tailrec
import scala.meta._
import scala.collection.immutable

private final case class TermCmdNode(cmd: TermCmd,
                                     params: immutable.Seq[TermParam],
                                     opts: immutable.Seq[TermOpt],
                                     children: immutable.Seq[TermCmdNode])

private final case class TermArgGraph(commands: immutable.Seq[TermCmdNode],
                                      topParams: immutable.Seq[TermParam],
                                      topOpts: immutable.Seq[TermOpt])

private object TermCmdNode {
  implicit val definable: Definable[TermCmdNode] = (a: TermCmdNode) => recDefine(a)

  private def recDefine(a: TermCmdNode): Term = {
    val entityVal = q"val entity = ${a.cmd.arg}"
    val paramsVal = q"val params = immutable.Seq(..${a.params.map(_.defnRuntimeTerm)})"
    val optsVal = q"val opts = immutable.Seq(..${a.opts.map(_.defnRuntimeTerm)})"
    val childrenVal = a.children match {
      case Nil => q"val children = immutable.Seq.empty[CmdNode]"
      case cdren => q"val children = immutable.Seq(..${cdren.map(recDefine)})"
    }

    q"""import _root_.scala.collection.immutable
        new CmdNode{
          $entityVal
          $paramsVal
          $optsVal
          $childrenVal
        }"""
  }
}