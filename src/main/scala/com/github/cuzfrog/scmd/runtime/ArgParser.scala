package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.internal.SimpleLogger

import scala.annotation.tailrec


private object ArgParser {
  def parse(argTree: ArgTree, args: Seq[String]): Seq[Node] = {
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
private class BacktrackingParser(argTree: ArgTree, args: Seq[String]) extends SimpleLogger{

  import BacktrackingParser._

  private[this] implicit val c: Context = Context(argTree, args.map(categorize))
  private[this] val topPath: TryPath = TryPath(c.anchor(c.getCurrentCmdNode))
  /**
    * Parse args against client defined argTree,
    * return a new tree containing value and consisting of only the right path.
    */
  def parse: Seq[Node] = {
    recProceed(topPath)
    trace(s"Parsed path:\n${topPath.prettyString}")
    topPath.convertTo[Seq[Node]]
  }

  private def checkIfUniquePath(path: TryPath): Unit = {
    path.toTop.findUnsealedFork match { //check if it's a single path through.
      case None => ()
      case Some(p) =>
        val forks = p.getBranches
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
  protected final def recProceed(currentPath: TryPath)(implicit c: Context): TryPath = {
    proceedOne(c) match {
      case Some(ae) =>
        ae match {
          //if return possible anchors:
          case Right(anchors) =>
            val forks = anchors.map(TryPath(_)).map(currentPath.pipeAddFork)
            val bottomPath = forks.lastOption
              .getOrElse(throw new AssertionError("Returned anchors should not be empty."))
            recProceed(bottomPath)
          //if this path is an end(exception occurred):
          case Left(e) =>
            currentPath.backtrack match {
              //found an unexplored fork:
              case Some(path) =>
                c.restore(path.anchor.contextSnapshot)
                recProceed(path)
              //backtrack end:
              case None =>
                if (currentPath.toTop.isComplete) {
                  currentPath
                }
                else { //finally path is not complete, parsing failed:
                  throw e
                }
            }
        }
      //if complete:
      case None =>
        currentPath.complete //seal this path.
        currentPath.toTop.findUnsealedFork match { //if there's another unsealed fork:
          case None => currentPath //all paths are sealed, return
          case Some(path) =>
            c.restore(path.anchor.contextSnapshot)
            recProceed(path)
        }
    }
  }

  private def proceedOne(implicit c: Context): Option[AnchorEither] = {
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
      case LongOptExtractor(lOpt, _, _) => LongOpt(lOpt)
      case paramOrCmd => ParamOrCmd(paramOrCmd)
    }
    TypedArg(cateArg, arg)
  }
}


