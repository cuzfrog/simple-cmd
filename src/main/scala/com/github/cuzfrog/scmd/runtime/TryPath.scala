package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.ScmdUtils._
import com.github.cuzfrog.scmd.runtime.logging.TryPathLogging

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Anchors represent every successful/possible parsing result. They form paths of parsing.
  */
private[runtime] sealed trait Anchor {
  /** Return the (first) node. */
  def node: Node
  /** Return the (first) context snapshot. */
  def contextSnapshot: ContextSnapshot

  def anchors: Seq[Anchor]
}
private[runtime] case class SingleAnchor(node: Node,
                                         contextSnapshot: ContextSnapshot) extends Anchor {
  override def anchors: Seq[SingleAnchor] = Seq(this)
}
private[runtime] case class MultiAnchor(anchors: Seq[Anchor]) extends Anchor {
  require(anchors.nonEmpty, "Multi-Anchor must have at least one element.")
  def node: Node = anchors.head.node
  def contextSnapshot: ContextSnapshot = anchors.head.contextSnapshot
}
private[runtime] case object NoneAnchor extends Anchor {
  override def node: Node = notSupported
  override def contextSnapshot: ContextSnapshot = notSupported
  override def anchors: Seq[Anchor] = notSupported
  private def notSupported = throw new UnsupportedOperationException
}

/**
  * Paths represent a tree of any possible parsing trail. With context snapshot preserved.
  *
  * Not thread-safe. It should be only accessed inside ArgParser.
  */
private class TryPath(argAnchor: Anchor) {
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
    recBacktrack(this)
  }
  @tailrec
  private def recBacktrack(currentTop: TryPath): Option[TryPath] = {
    currentTop.parentOpt match {
      case Some(parent) =>
        if (parent.branches.isEmpty) {
          throw new AssertionError("Forks have not been put into anchor." +
            "Children not in parent's branches." +
            s"Of node: ${this.anchor.node.prettyString}")
        } else if (parent.branches.lengthCompare(1) == 0) {
          recBacktrack(parent)
        } else { //forks > 1
          parent.branches -= currentTop
          parent.branches.filter(!_.isComplete).flatMap(recGetIncompleteChild).headOption
        }
      case None => None
    }
  }
  //contract: current.isComplete == false
  private def recGetIncompleteChild(current: TryPath): Option[TryPath] = {
    current.branches.filter(!_.isComplete) match {
      case arr if arr.isEmpty => Some(current) //no child.
      case arr => arr.flatMap(recGetIncompleteChild).headOption
    }
  }

  /**
    * Move upstream, and remove any other branches exception current one.
    *
    * @return the same Path with any redundant forks trimmed, only single lineage left.
    */
  def trimUpstream: this.type = {
    if (!this.branches.contains(TryPath.CompletePath))
      throw new UnsupportedOperationException("def trimUpstream can only be called on down stream end.")
    @tailrec
    def recTrimUpstream(tryPath: TryPath): this.type = {
      tryPath.parentOpt match {
        case Some(parent) =>
          parent.branches.clear()
          parent.branches += tryPath
          recTrimUpstream(parent)
        case None => this
      }
    }
    recTrimUpstream(this)
  }

  /**
    * Filter TryPaths that satisfy the prediction f.
    * Top path will not suffer the filtering.
    *
    * @return the top path with downstream paths filtered.
    */
  def filter(f: TryPath => Boolean): TryPath = {
    val top = this.toTop
    def recFilterDownstream(f: TryPath => Boolean, currentPath: TryPath): Unit = {
      if (!currentPath.branches.contains(TryPath.CompletePath)) {
        val filtered = currentPath.branches.filter(f)
        currentPath.branches.clear()
        currentPath.branches ++= filtered
        if (currentPath.branches.isEmpty) currentPath.complete
        currentPath.branches.foreach(tp => recFilterDownstream(f, tp))
      }
    }
    recFilterDownstream(f, top)
    top
  }

  /** Return top parent of this path. If this is already at top, return this. */
  @inline
  def toTop: TryPath = TryPath.getTop(this)

  /** From this, downstream, try to find an end path that is not complete. */
  def findUnsealedFork: Option[TryPath] = this.recFindUnsealedFork
  private def recFindUnsealedFork: Option[TryPath] = {
    if (this.isComplete) None
    else {
      this.branches match {
        case arr if arr.isEmpty => Some(this) //end with empty, not complete
        case arr => arr.flatMap(_.recFindUnsealedFork).headOption
      }
    }
  }

  def getBranches: Seq[TryPath] = this.branches map identity

  /** Check if the path is unique. Return path with multiple branches. */
  @inline
  def checkUniqueness: Option[TryPath] = {
    if (!this.isComplete) throw new AssertionError("Path not complete yet.")
    @tailrec
    def checkUniqueness(path: TryPath): Option[TryPath] = {
      path.branches match {
        case arr if arr.contains(TryPath.CompletePath) => None
        case arr if arr.size == 1 => checkUniqueness(arr.head)
        case _ => Some(path)
      }
    }
    checkUniqueness(this.toTop)
  }

  /** Return headOption of sub-branches. */
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
      case arr => arr.map(_.isComplete).forall(identity)
    }
  }
}

private object TryPath {

  def apply(argAnchor: => Anchor): TryPath = new TryPath(argAnchor) with TryPathLogging

  private case object CompletePath extends TryPath(NoneAnchor) {
    override def anchor: Anchor =
      throw new UnsupportedOperationException(s"CompletePath's anchor is empty.")
  }

  implicit val convert2nodeSeq: Convertible[TryPath, Seq[(Node, ContextSnapshot)]] =
    new Convertible[TryPath, Seq[(Node, ContextSnapshot)]] {
      override def convertTo(a: TryPath): Seq[(Node, ContextSnapshot)] = {
        val top = a.toTop
        recConvert(top, Seq())
      }
    }

  @tailrec private
  def recConvert(p: TryPath, acc: Seq[(Node, ContextSnapshot)]): Seq[(Node, ContextSnapshot)] = {
    val elems = p.anchor.anchors.map { n => n.node -> n.contextSnapshot }
    p.branches.headOption match {
      case Some(path) if path != CompletePath =>
        recConvert(path, acc ++ elems)
      case _ => acc ++ elems
    }
  }


  implicit val canFormPrettyString: CanFormPrettyString[TryPath] =
    new CanFormPrettyString[TryPath] {
      override def mkPrettyString(a: TryPath): String = {
        val top = a.toTop
        recMkPrettyString(top).mkString(System.lineSeparator)
      }
    }
  private def recMkPrettyString(p: TryPath, indent: String = ""): Seq[String] = {
    val thisP = p.anchor.anchors.map { anchr =>
      anchr.node match {
        case n: CmdNode => s"${indent}cmd - ${n.entity.name}"
        case n: PriorNode => s"${indent}prior - ${n.entity.name}"
        case n: ParamNode[_] =>
          s"${indent}param : ${n.entity.name}[${n.tpe}] - ${n.value}"
        case n: OptNode[_] =>
          s"${indent}opt : ${n.entity.name}[${n.tpe}] - ${n.value}"
        case n: PropNode[_] =>
          s"${indent}prop : ${n.entity.name}[${n.tpe}] - ${n.value.mkString("|")}"
      }
    }
    val subs = if (!(p.branches.isEmpty || p.branches.contains(CompletePath))) {
      p.branches.flatMap(p => recMkPrettyString(p, indent + " "))
    } else Nil
    thisP ++ subs
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
