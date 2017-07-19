package com.github.cuzfrog.scmd.parse

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

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
  private[this] var parentOpt: Option[Path] = None
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
