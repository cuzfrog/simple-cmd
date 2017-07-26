package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.internal.SimpleLogger

private trait ContextLogging extends Context with SimpleLogger {
  override implicit val loggerAgent = classOf[Context].getName

  abstract override def anchor(n: Node): Anchor = {
    debug(s"anchor for node:${n.prettyString}")
    super.anchor(n)
  }

  abstract override def nodeAdvance(cmdName: String): Option[CmdNode] = {
    val result = super.nodeAdvance(cmdName)
    result match {
      case Some(cmdNode) => debug(s"advance to ${cmdNode.prettyString}")
      case None => debug(s"advance to cmd node failed.")
    }
    result
  }

  //  abstract override def nextParamNode: Option[ParamNode[_]] = {
  //    debug(s"Try next params. " +
  //      s"current of cmdNode: ${getCurrentCmdNode.params.size}; cursor:$getParamCursor")
  //    super.nextParamNode
  //  }
}


private trait TryPathLogging extends TryPath with SimpleLogger {
  override implicit val loggerAgent = classOf[TryPath].getName

  override def complete: TryPathLogging.this.type = {
    val thisPath = super.complete
    debug(s"Path complete: ${thisPath.anchor.node.prettyString}")
    thisPath
  }
}