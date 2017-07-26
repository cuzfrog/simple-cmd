package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.Empty
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
private class TryPath(argAnchor: => Anchor) {
  private var parentOpt: Option[TryPath] = None
  private val branches: mutable.ArrayBuffer[TryPath] = mutable.ArrayBuffer.empty

  private def setParent(path: TryPath): this.type = {
    this.parentOpt = Some(path)
    this
  }

  def anchor: Anchor = argAnchor

  /** Pipe a Path in and add it as a fork, return the same Path passed in. */
  def pipeAddFork(path: TryPath): TryPath = {
    if (this.isComplete) throw new AssertionError("Completed path should not add fork." +
      s"Of node: ${this.anchor.node.prettyString}")
    path.setParent(this)
    this.branches += path
    path
  }

  /**
    * Search upstream, until find another sibling fork. And remove current ill-fated fork.
    *
    * @return an Option of unexplored Path.
    */
  def backtrack: Option[TryPath] = {
    @tailrec
    def recBacktrack(currentTop: TryPath): Option[TryPath] = {
      parentOpt match {
        case Some(parent) =>
          if (parent.branches.size < 1) {
            throw new AssertionError("Forks have not been put into anchor." +
              "Children not in parent's branches." +
              s"Of node: ${this.anchor.node.prettyString}")
          } else if (parent.branches.size == 1) {
            recBacktrack(parent)
          } else { //forks > 1
            parent.branches -= currentTop
            parent.branches.lastOption
          }
        case None => None
      }
    }
    recBacktrack(this)
  }

  /** Return top parent of this path. If this is already at top, return this. */
  @inline
  def toTop: TryPath = TryPath.getTop(this)

  /** From this, downstream, try to find an end path that is not complete. */
  def findUnsealedFork: Option[TryPath] = {
    if (this.isComplete) None
    else {
      this.branches match {
        case arr if arr.isEmpty => Some(this) //end with empty, not complete
        case arr => arr.flatMap(_.findUnsealedFork).headOption
      }
    }
  }

  def getBranches: Seq[TryPath] = this.branches map identity

  /** From this, downstream,
    * cut forks that end with exception and are not long enough. */
  def prune: this.type = {

    def recPrune = {
      this.findUnsealedFork match {
        case None =>
        case Some(path) =>
      }
    }
    ???
    this
  }

  def nextHeadOption: Option[TryPath] = this.branches.headOption

  /** Seal this path by adding a CompletePath to its branches. */
  @throws[AssertionError]("when this path already has non-empty branches.")
  def complete: this.type = {
    if (this.branches.nonEmpty)
      throw new AssertionError("Path to complete has non-empty branches." +
        s"Of node: ${this.anchor.node.prettyString}")
    this.branches += TryPath.CompletePath
    this
  }

  /** Check recursively if this path is complete,
    * by check if all its branches end with CompletePath. */
  def isComplete: Boolean = {
    this.branches match {
      case arr if arr.isEmpty => false
      case arr if arr.contains(TryPath.CompletePath) => true
      case arr => arr.map(_.isComplete).forall(_ == true)
    }
  }
}

private object TryPath {

  def apply(argAnchor: => Anchor): TryPath = new TryPath(argAnchor) with TryPathLogging

  private case object CompletePath extends TryPath(Empty)

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