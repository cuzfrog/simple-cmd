package com.github.cuzfrog.scmd.runtime.logging

import com.github.cuzfrog.scmd.internal.SimpleLogging
import com.github.cuzfrog.scmd.runtime.TryPath
import com.github.cuzfrog.scmd.ScmdUtils._

private[runtime] trait TryPathLogging extends TryPath with SimpleLogging {
  override lazy val loggerAgent: SimpleLogging.LoggerAgent = classOf[TryPath].getName

  override def complete: TryPathLogging.this.type = {
    val thisPath = super.complete
    val cs = thisPath.anchor.contextSnapshot
    debug(s"Path complete: ${thisPath.anchor.node.prettyString} and" +
      s" ${thisPath.anchor.anchors.size} more|" +
      s"${cs.argCursor} -> '${cs.rudeArg}'")
    //trace(s"After complete path:\n${super.toTop.prettyString}")
    thisPath
  }

  override def backtrack: Option[TryPath] = {
    //trace(s"Before backtrack path:\n${super.toTop.prettyString}")
    val result = super.backtrack
    result match {
      case Some(p) => debug(s"Backtrack to path of node:${p.anchor.node.prettyString}")
      case None => debug(s"Backtrack end. Last failed node:${super.anchor.node.prettyString}")
    }
    //trace(s"After backtrack path:\n${super.toTop.prettyString}")
    result
  }

  abstract override def findUnsealedFork: Option[TryPath] = {
    val result = super.findUnsealedFork
    result match {
      case Some(b) => debug(s"find unsealed path: ${b.anchor.node.prettyString}")
      case None => debug(s"Path sealed.")
    }
    result
  }

  abstract override def pipeAddFork(path: TryPath): TryPath = {
    debug(s"Add fork '${path.anchor.node.prettyString}' to path: ${this.anchor.node.entity.name}")
    super.pipeAddFork(path)
  }
}
