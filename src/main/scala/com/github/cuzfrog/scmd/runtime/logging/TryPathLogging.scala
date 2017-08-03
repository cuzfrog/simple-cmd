package com.github.cuzfrog.scmd.runtime.logging

import com.github.cuzfrog.scmd.internal.SimpleLogging
import com.github.cuzfrog.scmd.runtime.TryPath

private[runtime] trait TryPathLogging extends TryPath with SimpleLogging {
  override lazy val loggerAgent = classOf[TryPath].getName

  override def complete: TryPathLogging.this.type = {
    val thisPath = super.complete
    debug(s"Path complete: ${thisPath.anchor.node.prettyString}")
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
}
