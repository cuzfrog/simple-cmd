package com.github.cuzfrog.scmd.runtime.logging

import com.github.cuzfrog.scmd.internal.{IgnoreLogging, SimpleLogging}
import com.github.cuzfrog.scmd.runtime.{Anchor, CmdNode, Context, Node, ParamNode}
import com.github.cuzfrog.scmd.ScmdUtils._

private[runtime] trait ContextLogging extends Context with SimpleLogging {
  override lazy val loggerAgent = classOf[Context].getName

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

  @IgnoreLogging
  abstract override def nextParamNode: Option[ParamNode[_]] = {
    debug(s"Try next params. " +
      s"current of cmdNode: ${getCurrentCmdNode.params.size}; cursor:$getParamCursor")
    super.nextParamNode
  }
}


