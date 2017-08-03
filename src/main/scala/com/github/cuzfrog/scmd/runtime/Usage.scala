package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.internal.AnsiFormatter.FormattedHelper

private[runtime] sealed trait ConsoleType
private[runtime] object ConsoleType {
  def detect: ConsoleType = new ConsoleType {} //todo: detect console type.
}


private[runtime] trait ManualEvidence[A] {
  def genManual(a: A)(implicit consoleType: ConsoleType): String
}
private object ManualEvidence {
  implicit val GNU_ManualEvidence: ManualEvidence[ArgTree] = new ManualEvidence[ArgTree] {
    override def genManual(a: ArgTree)(implicit consoleType: ConsoleType): String = {
      ansi"%bold{Description}"
      ???
    }
  }


}