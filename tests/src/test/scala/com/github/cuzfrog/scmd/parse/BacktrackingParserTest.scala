package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd._
import org.junit._
import org.junit.Assert._

class BacktrackingParserTest {

  private val argTree = {
    val cmdEntry = new CmdEntryNode {
      override val entity = CommandEntry(name = "test-empty-cmdEntry")
      override val children = Seq(

      )
    }
    ArgTree(
      topParams = Seq(),
      topOpts = Seq(),
      cmdEntry = cmdEntry
    )
  }

  @Test
  def test1(): Unit = {
    val args = Array.empty[String]
    val result = new BacktrackingParserTestOne(argTree, args).parsedToPath
    println(result.prettyString)
  }
}


private class BacktrackingParserTestOne(argTree: ArgTree, args: Array[String])
  extends BacktrackingParser(argTree, Array()) {
  def parsedToPath: Path = this.recProceed()
}