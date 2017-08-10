package com.github.cuzfrog.scmd.runtime


package object console {
  private[runtime] implicit class UsageGenerationOps[A: UsageEvidence](a: A) {
    def genUsage(implicit consoleType: ConsoleType,
                 builder: StringBuilder = StringBuilder.newBuilder,
                 indent: Indent = Indent(2)): StringBuilder = {
      implicitly[UsageEvidence[A]].genUsage(a)
    }
  }

  private[runtime] implicit class ManualGenerationOps[A: ManualEvidence](a: A) {
    private implicit val consoleType: ConsoleType = ConsoleType.detect
    def genManual: String = implicitly[ManualEvidence[A]].genManual(a)
  }

  private[console] def alignOpts(seq: Seq[UsageOptNode]): Seq[UsageOptNode] = {
    if (seq.isEmpty) return Nil
    val maxAbbr = seq.map(_.abbr.length).max
    val maxName = seq.map(_.name.length).max
    seq.map { n =>
      val alAbbr = n.abbr + (" " * (maxAbbr - n.abbr.length))
      val alName = n.name + (" " * (maxName - n.name.length + 1))
      n.copy(abbr = alAbbr, name = alName)
    }
  }

  /** Align nodes by start place of description. */
  private[console] def align[N <: UsageNode](seq: Seq[N]): Seq[N] = {
    if (seq.isEmpty) return Nil
    val maxLHwidth = seq.map(_.descrLHwidth).max
    seq.map { n =>
      val offset = maxLHwidth - n.descrLHwidth + 1
      val result = n match {
        case n: UsageCmdNode=> n.copy(descrOffset = offset)
        case n: UsageParamNode => n.copy(descrOffset = offset)
        case n: UsageOptNode => n.copy(descrOffset = offset)
        case n: UsagePriorNode => n.copy(descrOffset = offset)
        case n: UsagePropNode => n.copy(descrOffset = offset)
      }
      result.asInstanceOf[N]
    }
  }
}
