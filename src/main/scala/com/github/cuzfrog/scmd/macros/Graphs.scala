package com.github.cuzfrog.scmd.macros

import scala.meta._

// ============ Raw(contains macro type) ==============

private final case class TermCmdNode(cmd: TermCmd,
                                     params: Seq[TermParam],
                                     opts: Seq[TermOpt],
                                     children: Seq[TermCmdNode])

private final case class TermArgGraph(commands: Seq[TermCmdNode], opts: Seq[TermOpt])
