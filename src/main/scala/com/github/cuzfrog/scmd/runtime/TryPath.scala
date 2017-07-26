package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.CanFormPrettyString

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Anchors represent every successful/possible parsing result. They form paths of parsing.
  */
private[runtime] case class Anchor(node: Node, contextSnapshot: ContextSnapshot)

/**
  * Paths represent a tree of any possible parsing trail.
  *
  * Not thread-safe. It should be only accessed inside ArgParser.
  */
private final case class TryPath(anchor: Anchor) {
  private var parentOpt: Option[TryPath] = None
  private val branches: mutable.ArrayBuffer[TryPath] = mutable.ArrayBuffer.empty

  private def setParent(path: TryPath): this.type = {
    this.parentOpt = Some(path)
    this
  }

  /** Pipe a Path in and add it as a fork, return the same Path passed in. */
  def pipeAddFork(path: TryPath): TryPath = {
    path.setParent(this)
    this.branches += path
    path
  }

  /**
    * Search upstream, until find another sibling fork. And remove current ill-fated fork.
    *
    * @return an Option of unexplored Path.
    */
  @tailrec
  def backtrack: Option[TryPath] = {
    parentOpt match {
      case Some(parent) =>
        if (parent.branches.size < 1) {
          throw new AssertionError("Forks have not been put into anchor.")
        } else if (parent.branches.size == 1) {
          parent.backtrack
        } else { //forks > 1
          parent.branches.remove(parent.branches.size - 1)
          parent.branches.lastOption
        }
      case None => None
    }
  }

  /** Return top parent of this path. If this is already at top, return this. */
  @inline
  def toTop: TryPath = TryPath.getTop(this)

  /** From this, downstream, try to find a path that has more than one sub-branches. */
  def findFork: Seq[TryPath] = {
    this.branches match {
      case arr if arr.isEmpty => Nil
      case arr if arr.size == 1 => arr.head.findFork
      case arr => arr map identity
    }
  }

  /** From this, downstream, cut forks that are not complete(end with exception and not long enough). */
  def prune: TryPath = {

  }

  def nextHeadOption: Option[TryPath] = this.branches.headOption
}

private object TryPath {

  implicit val convert2nodeSeq: Convertible[TryPath, Seq[Node]] = (a: TryPath) => {
    val top = a.toTop

    @tailrec
    def recConvert(p: TryPath, acc: Seq[Node]): Seq[Node] = {
      p.branches.headOption match {
        case None => acc :+ p.anchor.node
        case Some(path) => recConvert(path, acc :+ p.anchor.node)
      }
    }

    recConvert(top, Seq())
  }


  implicit val canFormPrettyString: CanFormPrettyString[TryPath] = (a: TryPath) => {
    val top = a.toTop

    def recMkPrettyString(p: TryPath): String = {
      if (p.branches.isEmpty) {
        p.anchor.node match {
          case cmdNode: CmdNode => s"cmd  [${cmdNode.entity.name}]"
          case paramNode: ParamNode[_] => s"param[${paramNode.entity.name}] - ${paramNode.value}"
          case optNode: OptNode[_] => s"opt  [${optNode.entity.name}] - ${optNode.value}"
          case cmdEntry: CmdEntryNode =>
            throw new AssertionError(s"CmdEntry should not be in path:${cmdEntry.entity}")
        }
      } else p.branches.map(recMkPrettyString).mkString(System.lineSeparator)
    }

    recMkPrettyString(top)
  }

  /** Get top Path from any given Path. */
  @tailrec
  private def getTop(a: TryPath): TryPath = {
    a.parentOpt match {
      case Some(parent) => getTop(parent)
      case None => a
    }
  }
}