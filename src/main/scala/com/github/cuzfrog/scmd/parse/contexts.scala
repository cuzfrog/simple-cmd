package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{ArgTree, CmdNode, ValueNode}

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
  private[this] val args = initArgs map identity //copy args
  private[this] var cursor: Int = 0

  /** Try to regress to parent cmd node and return it. */
  def nodeRegress: Option[CmdNode] = this.synchronized {
    currentCmdNode.parent.map { p => this.currentCmdNode = p; p }
  }

  /** Try to advance to a child cmd node and return it. */
  def nodeAdvance(cmdName: String): Option[CmdNode] = this.synchronized {
    currentCmdNode.children.find(_.entity.name == cmdName)
  }

  def getCurrentNode: CmdNode = this.currentCmdNode //return immutable object.

  /** Return current concerned arg, and set cursor to next. */
  def nextArg: Option[String] = this.synchronized {
    if (args.length <= cursor) None else {
      val result = args(cursor)
      cursor += 1
      Option(result)
    }
  }

  /** Return previously concerned arg, and set cursor back */
  def lastArg: Option[String] = this.synchronized {
    if (cursor <= 0) None else {
      cursor -= 1
      Option(args(cursor))
    }
  }

  def takeSnapshot: ContextSnapshot = this.synchronized {
    ContextSnapshot(currentCmdNode, cursor)
  }
}

private case class ContextSnapshot(cmdNode: CmdNode, cursor: Int)
private object ContextSnapshot {
  implicit def takeSnapshot(context: Context): ContextSnapshot = context.takeSnapshot
}

private case class ValueAnchor(valueNode: ValueNode, contextSnapshot: ContextSnapshot)