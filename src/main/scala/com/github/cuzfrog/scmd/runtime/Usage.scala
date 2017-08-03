package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.internal.AnsiFormatter.FormattedHelper

private[runtime] sealed trait ConsoleType
private[runtime] object ConsoleType {
  def detect: ConsoleType = new ConsoleType {} //todo: detect console type.
}


private[runtime] trait UsageEvidence[A] {
  def genUsage(a: A)(implicit consoleType: ConsoleType): String
}
private object UsageEvidence {
  implicit val defaultUsage: UsageEvidence[ArgTree] = new UsageEvidence[ArgTree] {
    override def genUsage(a: ArgTree)(implicit consoleType: ConsoleType): String = {
      ansi"%bold{Description}"
      ???
    }
  }


}