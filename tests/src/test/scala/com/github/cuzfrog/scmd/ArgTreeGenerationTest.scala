package com.github.cuzfrog.scmd

import java.nio.file.Path

import org.junit._
import org.junit.Assert._

class ArgTreeGenerationTest {

  @ScmdDefTest
  class CpDef {
    val src =
      paramDef[Seq[Path]](description = "Paths of files/dirs to copy.", isMandatory = true)
    val dst =
      paramDef[Path](description = "Path (of dir) to copy to.", isMandatory = true)
    val recursive = optDef[Boolean](description = "Copy dirs recursively", abbr = "R")
  }

  @Test
  def cpTest(): Unit = {
    val cpDef = new CpDef
    println(cpDef.argTree.prettyString)
  }
}
