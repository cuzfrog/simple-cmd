package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{ArgTree, CmdNode, ValueNode}

import scala.collection.mutable

/**
  * Stateful OOP object representing parsing process.
  *
  * Design to be thread-safe by synchronizing actions.
  */
private class Context(argTree: ArgTree, initArgs: Seq[String]) {

  @volatile private[this] var currentCmdNode: CmdNode = argTree.toTopNode
  private[this] val freshArgs = mutable.ArrayStack(initArgs: _*) //first arg on top
  private[this] val consumedArgs: mutable.ArrayStack[String] = mutable.ArrayStack.empty[String]

  /** Try to regress to parent cmd node and return it. */
  def nodeRegress: Option[CmdNode] = this.synchronized {
    currentCmdNode.parent.map { p => this.currentCmdNode = p; p }
  }

  /** Try to advance to a child cmd node and return it. */
  def nodeAdvance(cmdName: String): Option[CmdNode] = this.synchronized {
    this.currentCmdNode.children.find(_.entity.name == cmdName)
  }

  def getCurrentNode: CmdNode = this.currentCmdNode //return immutable object.

  /** Return current concerned arg, and set cursor to next. */
  def nextArg: Option[String] = this.synchronized {
    freshArgs.headOption.map { arg =>
      consumedArgs.push(arg); freshArgs.pop()
    }
  }

  /** Return previously concerned arg, and set cursor back */
  def lastArg: Option[String] = this.synchronized {
    consumedArgs.headOption.map { arg =>
      freshArgs.push(arg); consumedArgs.pop()
    }
  }
}

private case class ValueAnchor(valueNode: ValueNode, context: Context)