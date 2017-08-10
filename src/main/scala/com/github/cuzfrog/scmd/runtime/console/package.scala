package com.github.cuzfrog.scmd.runtime


package object console {
  private[runtime] implicit class UsageGenerationOps[A: UsageEvidence](a: A) {
    def genUsage(implicit consoleType: ConsoleType = ConsoleType.detect,
                 builder: StringBuilder = StringBuilder.newBuilder,
                 indent: Indent = Indent(2)): StringBuilder = {
      implicitly[UsageEvidence[A]].genUsage(a)
    }
  }

  private[runtime] implicit class ManualGenerationOps[A: ManualEvidence](a: A) {
    private implicit val consoleType: ConsoleType = ConsoleType.detect
    def genManual: String = implicitly[ManualEvidence[A]].genManual(a)
  }

  val APP_VERSION: String = Option(getClass.getPackage.getImplementationVersion).getOrElse("-dev")
}
