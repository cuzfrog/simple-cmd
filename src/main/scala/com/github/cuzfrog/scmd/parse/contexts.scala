package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{ArgTree, CmdNode, ParamNode, ValueNode}

import scala.collection.mutable
import scala.collection.immutable

/**
  * Stateful OOP object representing parsing process.
  *
  * Design to be thread-safe by synchronizing actions.
  * ArgTree needs to be immutable.
  */
private class Context(argTree: ArgTree, initArgs: Array[String]) {

  @volatile private[this] var currentCmdNode: CmdNode = argTree.toTopNode
  private[this] val paramCursors: mutable.Map[CmdNode, Int] = mutable.Map(currentCmdNode -> 0)

  private[this] val args = initArgs map identity //copy args
  private[this] var argCursor: Int = 0

  /** Try to regress to parent cmd node and return it. */
  def nodeRegress: Option[CmdNode] = this.synchronized {
    currentCmdNode.parent.map { p =>
      this.currentCmdNode = p; p
    }
  }

  /** Try to advance to a child cmd node and return it. */
  def nodeAdvance(cmdName: String): Option[CmdNode] = this.synchronized {
    currentCmdNode.children.find(_.entity.name == cmdName)
  }

  def getCurrentCmdNode: CmdNode = this.currentCmdNode //return immutable object.

  /** Return current pointed ParamNode of current CmdNode. */
  def nextParamNode: Option[ParamNode[_]] = this.synchronized {
    val cursor = paramCursors.getOrElse(currentCmdNode, 0)
    val params = currentCmdNode.params
    if (params.length <= cursor) None else {
      val result = params(cursor)
      paramCursors.put(currentCmdNode, cursor + 1)
      Option(result)
    }
  }

  /** Return previous pointed ParamNode of current CmdNode. */
  def lastParamNode: Option[ParamNode[_]] = this.synchronized {
    val cursor = paramCursors.getOrElse(currentCmdNode, 0)
    if (cursor <= 0) None else {
      val lastIdx = cursor - 1
      paramCursors.put(currentCmdNode, lastIdx)
      Option(currentCmdNode.params(lastIdx))
    }
  }

  /** Return current concerned arg, and set cursor to next. */
  def nextArg: Option[String] = this.synchronized {
    if (args.length <= argCursor) None else {
      val result = args(argCursor)
      argCursor += 1
      Option(result)
    }
  }

  /** Return previously concerned arg, and set cursor back */
  def lastArg: Option[String] = this.synchronized {
    if (argCursor <= 0) None else {
      argCursor -= 1
      Option(args(argCursor))
    }
  }

  def takeSnapshot: ContextSnapshot = this.synchronized {
    ContextSnapshot(currentCmdNode, argCursor, paramCursors.getOrElse(currentCmdNode, 0))
  }
}

private case class ContextSnapshot(cmdNode: CmdNode, argCursor: Int, paramCursor: Int)
private object ContextSnapshot {
  implicit def takeSnapshot(context: Context): ContextSnapshot = context.takeSnapshot
}

private case class ValueAnchor(valueNode: ValueNode, contextSnapshot: ContextSnapshot)