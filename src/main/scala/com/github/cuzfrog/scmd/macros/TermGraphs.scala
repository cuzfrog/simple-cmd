package com.github.cuzfrog.scmd.macros

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
  implicit val definable: Definable[TermCmdNode] = (a: TermCmdNode) => {
    q"""new CmdNode{
          val entity = ${a.cmd.arg}
          val params = Seq(..${a.params.map(_.defnRuntimeTerm.asInstanceOf[Term.Arg])})

        }"""
  }
}