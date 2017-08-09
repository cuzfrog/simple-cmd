package com.github.cuzfrog.scmd.runtime


package object console {
  private[runtime] implicit class UsageGenerationOps[A: UsageEvidence](a: A) {
    private val ev: UsageEvidence[A] = implicitly[UsageEvidence[A]]
    private implicit val consoleType: ConsoleType = ConsoleType.detect
    def genUsage(implicit builder: StringBuilder = StringBuilder.newBuilder): String = {
      ev.genUsage(a)
    }
  }

  private[runtime] implicit class ManualGenerationOps[A: ManualEvidence](a: A) {
    private val ev: ManualEvidence[A] = implicitly[ManualEvidence[A]]
    private implicit val consoleType: ConsoleType = ConsoleType.detect
    def genManual: String = ev.genManual(a)
  }
}
