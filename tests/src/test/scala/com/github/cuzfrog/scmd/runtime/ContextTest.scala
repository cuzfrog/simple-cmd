package com.github.cuzfrog.scmd.runtime

import org.junit._
import simulation.Cp

class ContextTestCp {
  private implicit val argTree: ArgTree = new Cp.CpDef(Nil).scmdRuntime.getArgTree
  private val args1 = List("src/file1", "src/file2", "dest/file", "-recursive")
    .map(BacktrackingParser.categorize)
  private val context = new Context(argTree, args1)
}
