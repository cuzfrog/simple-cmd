package com.github.cuzfrog.scmd.parse

import scala.annotation.tailrec


private object ArgParser {
  def parse(argTree: ArgTree, args: Array[String]): Seq[Node] = {
    new BacktrackingParser(argTree, args).parse
  }
}

/**
  * Parser that has full duty of matching ArgTree and passed-in args.
  * Entry of the whole parsing process.
  *
  * All instances should be created and forgotten only in this class.
  * That means no reference of instances used during parsing should escape this class.
  * After args have been parsed, all the instances created should be GCed.
  *
  * Not thread-safe. It should only be accessed inside ArgParser.
  */
private class BacktrackingParser(argTree: ArgTree, args: Array[String]) {

  import BacktrackingParser._

  private[this] implicit val c: Context = new Context(argTree, args.map(categorize).toSeq)
  private[this] var pathCursor: TryPath = TryPath(c.anchor(c.getCurrentCmdNode))

  /**
    * Parse args against client defined argTree,
    * return a new tree containing value and consisting of only the right path.
    */
  def parse: Seq[Node] = {
    val path = recProceed()
    val top = path.toTop
    top.findFork match { //check if it's a single path through.
      case Nil => top.convertTo[Seq[Node]]
      case forks =>
        val arg = try {
          args(forks.head.anchor.contextSnapshot.argCursor)
        } catch {
          case e: ArrayIndexOutOfBoundsException =>
            throw new AssertionError("Arg cursor resides out the bounds of array.")
        }

        val msg = forks.map(_.anchor.node).map {
          case cmdNode: CmdNode => cmdNode.entity.name
          case paramNode: ParamNode[_] => paramNode.entity.name
          case optNode: OptNode[_] =>
            throw new AssertionError(s"OptNode should not be ambiguous.[${optNode.entity.name}]")
          case cmdEntry: CmdEntryNode =>
            throw new AssertionError(s"CmdEntry should not be anchored.[$cmdEntry]")
        }
        throw new ArgParseException(s"Ambiguous arg: $arg for: ${msg.mkString("|")}", c)
    }
  }

  @tailrec
  protected final def recProceed(): TryPath = {
    proceedOne() match {
      case Some(ae) =>
        ae match {
          //if return possible anchors:
          case Right(anchors) =>
            val forks = anchors.map(TryPath.apply).map(pathCursor.pipeAddFork)
            pathCursor = forks.lastOption
              .getOrElse(throw new AssertionError("Returned anchors should not be empty."))
            recProceed()
          //if this path is an end(exception occurred):
          case Left(e) =>
            pathCursor.backtrack match {
              //found an unexplored fork:
              case Some(path) =>
                pathCursor = path
                c.restore(path.anchor.contextSnapshot)
                recProceed()
              //cannot backtrack to parent, parsing failed:
              case None => throw e
            }
        }
      //if complete:
      case None => pathCursor
    }
  }

  private def proceedOne(): Option[AnchorEither] = {
    if (c.isComplete) None
    else if (c.mandatoryLeftCnt > 0 && c.noArgLeft) {
      Some(ArgParseException("More args required", c))
    } else {
      c.nextCateArg.map(_.parsed)
    }
  }
}

private object BacktrackingParser {
  private val SingleOptExtractor = """-(\w{1}.*)""".r
  private val LongOptExtractor = """-((-[\w\d]+)+(=.*)?)""".r

  def categorize(arg: String): TypedArg[CateArg] = {
    val cateArg: CateArg = arg match {
      case SingleOptExtractor(sOpt) => SingleOpts(sOpt)
      case LongOptExtractor(lOpt) => LongOpt(lOpt)
      case paramOrCmd => ParamOrCmd(paramOrCmd)
    }
    TypedArg(cateArg, arg)
  }
}


