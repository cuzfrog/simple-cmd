package com.github.cuzfrog.scmd

import java.nio.file.Path

import org.junit._
import org.junit.Assert._

class ArgTreeGenerationTest {
  

  @ScmdDef
  class CpDef {
    //val badParam = paramDef[String](description = "throw exception")
    val cat = cmdDef(description = "Concatenate contents of files.")
    val files = paramDef[Seq[Path]](description = "Paths of files to concatenate.", isMandatory = true)
    val newLine = optDef[Boolean](description = "Add new line end to every file", abbr = "f")
  }

  @Test
  def test1(): Unit = {

  }
}
