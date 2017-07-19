package com.github.cuzfrog.scmd.parse

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Anchors represent every successful/possible parsing result. They form paths of parsing.
  */
private[parse] case class Anchor(node: Node, contextSnapshot: ContextSnapshot)

/**
  * Paths represent a tree of any possible parsing trail.
  *
  * Not thread-safe. It should be only accessed inside ArgParser.
  */
private final case class Path(anchor: Anchor) {
  private var parentOpt: Option[Path] = None
  private val forks: mutable.ArrayBuffer[Path] = mutable.ArrayBuffer.empty

  private def setParent(path: Path): this.type = {
    this.parentOpt = Some(path)
    this
  }

  /** Pipe a Path in and add it as a fork, return the same Path passed in. */
  def pipeAddFork(path: Path): Path = {
    path.setParent(this)
    this.forks += path
    path
  }

  /**
    * Search upstream, until find another sibling fork. And remove current ill-fated fork.
    *
    * @return an Option of unexplored Path.
    */
  @tailrec
  def backtrack: Option[Path] = {
    parentOpt match {
      case Some(parent) =>
        if (parent.forks.size < 1) {
          throw new AssertionError("Forks have not been put into anchor.")
        } else if (parent.forks.size == 1) {
          parent.backtrack
        } else { //forks > 1
          parent.forks.remove(parent.forks.size - 1)
          parent.forks.lastOption
        }
      case None => None
    }
  }
}

private object Path {
  implicit val convert2argTree: Convertible[Path, ArgTree] = (a: Path) => {
    ???
  }

  implicit val canFormPrettyString: CanFormPrettyString[Path] = (a: Path) => {
    val top = getTop(a)

    def recMkPrettyString(p: Path): String = {
      if (p.forks.isEmpty) {
        p.anchor.node match {
          case cmdNode: CmdNode => s"cmd  [${cmdNode.entity.name}]"
          case paramNode: ParamNode[_] => s"param[${paramNode.entity.name}] - ${paramNode.value}"
          case optNode: OptNode[_] => s"opt  [${optNode.entity.name}] - ${optNode.value}"
          case cmdEntry: CmdEntryNode =>
            throw new AssertionError(s"CmdEntry should not be in path:${cmdEntry.entity}")
        }
      } else p.forks.map(recMkPrettyString).mkString(System.lineSeparator)
    }

    recMkPrettyString(top)
  }

  /** Get top Path from any given Path. */
  @tailrec
  private def getTop(a: Path): Path = {
    a.parentOpt match {
      case Some(parent) => getTop(parent)
      case None => a
    }
  }
}