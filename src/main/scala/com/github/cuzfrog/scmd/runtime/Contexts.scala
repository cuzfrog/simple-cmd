package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.runtime.logging.ContextLogging

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Stateful OOP object representing parsing process.
  *
  * Notice most methods have side-effects.
  *
  * Design to be thread-safe by synchronizing actions.
  * So that later concurrent parsing may be achieved in some degree.
  * ArgTree needs to be immutable.
  */
private[runtime] class Context(argTree: ArgTree, args: Seq[TypedArg[CateArg]]) {
  require(args.nonEmpty, "Context construct failed because of empty args.")
  private[this] val topNode: CmdNode = argTree.toTopNode
  @volatile private[this] var currentCmdNode: CmdNode = topNode
  /** Parameter is ordered. */
  private[this] var paramCursor: Int = 0
  /**
    * Not yet consumed opts accumulated upstream.
    * OptNode's equality depends on OptionArg's.
    * Opts are out-of-ordered and can be put at tail.
    */
  private[this] val optsUpstreamLeft: mutable.ArrayBuffer[OptNode[_]] =
    mutable.ArrayBuffer(topNode.opts: _*)
  private[this] val propsRepo: mutable.Set[PropNode[_]] = mutable.Set(argTree.props: _*)
  private[this] var argCursor: Int = 0
  private[this] var currentCateArg: TypedArg[CateArg] = args.head

  /** Try to regress to parent cmd node and return it. */
  //  def nodeRegress: Option[CmdNode] = this.synchronized {
  //    currentCmdNode.parent.map { p =>
  //      this.currentCmdNode = p; p
  //    }
  //  }

  /** Try to advance to a child cmd node and return it. */
  def nodeAdvance(cmdName: String): Option[CmdNode] = this.synchronized {
    currentCmdNode.subCmdEntry.children.find(_.entity.name == cmdName) map { n =>
      optsUpstreamLeft ++= n.opts //register opt nodes.
      paramCursor = 0 //reset param cursor
      currentCmdNode = n //set cmd node
      n
    }
  }

  def getCurrentCmdNode: CmdNode = this.currentCmdNode //return immutable object.
  def getParamCursor: Int = paramCursor
  def getCurrentArg: TypedArg[CateArg] = currentCateArg

  /** Return not yet consumed opts accumulated upstream. */
  def getUpstreamLeftOpts: Seq[OptNode[_]] = this.synchronized {
    Seq(optsUpstreamLeft: _*) //OptNode is immutable.
  }

  /** Consume a Node and produce an Anchor. */
  @inline
  def anchors(ns: Node*): Seq[Anchor] = this.synchronized {ns.map(anchor)}

  /** Create an anchor, if the node is an opt, register it as consumed. */
  @inline
  def anchor(n: Node): Anchor = this.synchronized {
    val updatedNode: Node = n match {
      case optNode: OptNode[_] =>
        if (!optNode.isVariable) optsUpstreamLeft -= optNode //register consumed
        n //todo: deal with variable value opt
      case propNode: PropNode[_] =>
        val storedPropNode = propsRepo.find(_ == propNode)
          .getOrElse(throw new AssertionError(s"PropNode not in context:$propNode"))
        val updated = storedPropNode.copy(value = storedPropNode.value ++ propNode.value)
        propsRepo.remove(storedPropNode) //this is required, because updated node is equal to storedPropNode
        propsRepo.add(updated)
        updated
      case otherNode => otherNode //nothing needed to do.
    }
    Anchor(updatedNode, this)
  }

  /** Return current pointed ParamNode of current CmdNode. */
  @inline
  def nextParamNode: Option[ParamNode[_]] = this.synchronized {
    val params = currentCmdNode.params
    if (noMoreParamForThisCmd) None else {
      val result = params(paramCursor)
      paramCursor += 1
      Option(result)
    }
  }

  /** Return previous pointed ParamNode of current CmdNode. */
  def lastParamNode: Option[ParamNode[_]] = this.synchronized {
    if (paramCursor <= 0) None else {
      paramCursor -= 1
      Option(currentCmdNode.params(paramCursor))
    }
  }

  /** Return current concerned arg, and set cursor to next. */
  def nextCateArg: Option[CateArg] = this.synchronized {
    if (noArgLeft) None else {
      val result = args(argCursor)
      argCursor += 1
      currentCateArg = result
      Option(result.typedArg)
    }
  }

  /** Return current concerned arg, and set cursor to next. */
  def nextArg: Option[String] = nextCateArg.map(_.arg)

  /** Return current concerned arg of given type, if successful, set cursor to next. */
  def nextArgWithType[T <: CateArg : ClassTag]: Option[String] = this.synchronized {
    if (noArgLeft) None else {
      val result = args(argCursor).typedArg
      Option(result).collect {
        case catArg if catArg.getClass == implicitly[ClassTag[T]].runtimeClass =>
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
    ContextSnapshot(currentCmdNode,
      optsUpstreamLeft map identity,
      argCursor,
      this.getCurrentArg.rude,
      paramCursor)
  }

  /** Restore context to the state of a given snapshot. */
  def restore(snapshot: ContextSnapshot): Unit = this.synchronized {
    currentCmdNode = snapshot.cmdNode
    optsUpstreamLeft.clear()
    optsUpstreamLeft ++= snapshot.optsUpstreamLeft
    argCursor = snapshot.argCursor
    paramCursor = snapshot.paramCursor
  }

  @inline
  def mandatoryLeftCnt: Int = this.synchronized {
    val paramCnt = currentCmdNode.params.drop(paramCursor).count(_.entity.isMandatory)
    val optCnt = optsUpstreamLeft.filter(_.value.isEmpty).count(_.entity.isMandatory)
    val subCmdCnt = currentCmdNode.subCmdEntry.mandatoryDownstreamCnt
    paramCnt + optCnt + subCmdCnt
  }

  @inline
  def isComplete: Boolean = this.synchronized {mandatoryLeftCnt == 0 && noArgLeft}

  def noArgLeft: Boolean = this.synchronized(args.length <= argCursor)

  private def noMoreParamForThisCmd: Boolean =
    this.synchronized(currentCmdNode.params.length <= paramCursor)
}

private object Context {
  def apply(argTree: ArgTree, args: Seq[TypedArg[CateArg]]): Context =
    new Context(argTree, args) with ContextLogging
}

private case class ContextSnapshot(cmdNode: CmdNode,
                                   optsUpstreamLeft: Seq[OptNode[_]],
                                   argCursor: Int,
                                   rudeArg: String,
                                   paramCursor: Int)
private object ContextSnapshot {
  implicit def takeSnapshot(context: Context): ContextSnapshot = context.takeSnapshot
}

private case class TypedArg[+A <: CateArg](typedArg: A, rude: String)

