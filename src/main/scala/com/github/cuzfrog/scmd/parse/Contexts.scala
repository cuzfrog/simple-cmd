package com.github.cuzfrog.scmd.parse

import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag

/**
  * Stateful OOP object representing parsing process.
  *
  * Notice most methods have side-effects.
  *
  * Design to be thread-safe by synchronizing actions.
  * ArgTree needs to be immutable.
  */
private[parse] class Context(argTree: ArgTree, args: Seq[TypedArg[CateArg]]) {

  @volatile private[this] var currentCmdNode: CmdNode = argTree.toTopNode
  private[this] val paramCursors: mutable.Map[CmdNode, Int] = mutable.Map(currentCmdNode -> 0)
  //private[this] val consumedOpts: mutable.Map[OptNode[_], String] = mutable.Map.empty

  private[this] var argCursor: Int = 0

  /** Try to regress to parent cmd node and return it. */
  def nodeRegress: Option[CmdNode] = this.synchronized {
    currentCmdNode.parent.map { p =>
      this.currentCmdNode = p; p
    }
  }

  /** Try to advance to a child cmd node and return it. */
  def nodeAdvance(cmdName: String): Option[CmdNode] = this.synchronized {
    currentCmdNode.subCmdEntry.children.find(_.entity.name == cmdName)
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
  def nextCateArg: Option[CateArg] = this.synchronized{
    if (noArgLeft) None else {
      val result = args(argCursor)
      argCursor += 1
      Option(result.typedArg)
    }
  }

  /** Return current concerned arg, and set cursor to next. */
  def nextArg: Option[String] = nextCateArg.map(_.arg)

  /** Return current concerned arg of given type, if successful, set cursor to next. */
  def nextArgWithType[T <: CateArg]: Option[String] = this.synchronized {
    if (noArgLeft) None else {
      val result = args(argCursor).typedArg
      Option(result).collect { case catArg: T =>
        argCursor += 1
        catArg.arg
      }
    }
  }

  /** Return previously concerned arg, and set cursor back */
  def lastArg: Option[String] = this.synchronized {
    if (argCursor <= 0) None else {
      argCursor -= 1
      Option(args(argCursor).typedArg.arg)
    }
  }

  def takeSnapshot: ContextSnapshot = this.synchronized {
    ContextSnapshot(currentCmdNode, argCursor, paramCursors.getOrElse(currentCmdNode, 0))
  }

  def restore(snapshot: ContextSnapshot): Unit = ???

  /** Count mandatory args downstream. */
  def mandatoryArgDefsLeftCnt: Int = {
    val paramCursor = paramCursors.getOrElse(currentCmdNode, 0)
    val paramCnt = currentCmdNode.params.drop(paramCursor).count(_.entity.isMandatory)
    paramCnt + currentCmdNode.subCmdEntry.mandatoryCnt
  }

  def isComplete: Boolean = mandatoryArgDefsLeftCnt == 0 && noArgLeft
  def noArgLeft: Boolean = args.length <= argCursor
}

private case class ContextSnapshot(cmdNode: CmdNode, argCursor: Int, paramCursor: Int)
private object ContextSnapshot {
  implicit def takeSnapshot(context: Context): ContextSnapshot = context.takeSnapshot
}

private case class TypedArg[+A <: CateArg](typedArg: A, rude: String)

private[parse] case class Anchor[+N: ClassTag](node: N,
                                               contextSnapshot: ContextSnapshot,
                                               parent: Option[Anchor[_]] = None,
                                               forks: Seq[Anchor[_]] = Nil)

