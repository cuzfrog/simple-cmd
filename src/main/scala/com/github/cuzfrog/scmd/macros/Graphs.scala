package com.github.cuzfrog.scmd.macros

import scala.meta._

private final case class TermCmdNode(cmd: TermCmd,
                                     params: Seq[TermParam],
                                     opts: Seq[TermOpt],
                                     children: Seq[TermCmdNode])

private final case class TermArgGraph(commands: Seq[TermCmdNode],
                                      topParams: Seq[TermParam],
                                      topOpts: Seq[TermOpt])
