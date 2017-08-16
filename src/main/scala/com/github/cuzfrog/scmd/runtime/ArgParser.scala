package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.ScmdUtils._
import com.github.cuzfrog.scmd.internal.SimpleLogging

import scala.annotation.tailrec
import scala.language.reflectiveCalls
import scala.util.matching.Regex

private object ArgParser {
  @throws[ArgParseException]("when parsing failed.")
  def parse(argTree: ArgTree, args: Seq[String]): Seq[(Node, ContextSnapshot)] = {
    new BacktrackingParser(args)(argTree).parse
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
private class BacktrackingParser(args: Seq[String])(implicit argTree: ArgTree) extends SimpleLogging {
  if (args.isEmpty)
    throw ArgParseException("No args specified.", ContextSnapshot.preContextSnapshot(argTree))

  import BacktrackingParser._

  private[this] implicit val c: Context = Context(argTree, args.map(categorize))
  private[this] val topPath: TryPath = TryPath(c.anchor(c.getCurrentCmdNode))
  /**
    * Parse args against client defined argTree,
    * return a new tree containing value and consisting of only the right path.
    */
  def parse: Seq[(Node, ContextSnapshot)] = {
    recProceed(topPath, Seq())
    checkIfUniquePath(topPath)
    trace(s"\nParsed path:--------\n${topPath.prettyString}\n-------------Path end.")
    topPath.convertTo[Seq[(Node, ContextSnapshot)]]
  }

  private def checkIfUniquePath(path: TryPath): Unit = {
    path.toTop.checkUniqueness match { //check if it's a single path through.
      case None => ()
      case Some(p) =>
        val forks = p.getBranches
        val arg = forks.head.anchor.contextSnapshot.rudeArg

        val msg = forks.map(_.anchor.node).map {
          case cmdNode: CmdNode => cmdNode.entity.name
          case paramNode: ParamNode[_] => paramNode.entity.name
          case n@(_: OptNode[_] | _: PropNode[_] | _: PriorNode) =>
            throw new AssertionError(s"OptNode/PropNode should not be ambiguous.[${n.entity.name}]")
        }
        throw ArgParseException(s"Ambiguous arg: $arg for: ${msg.mkString(",")}", c)
    }
  }


  @tailrec
  protected final def recProceed(currentPath: TryPath,
                                 exceptions: Seq[ArgParseException])
                                (implicit c: Context): TryPath = {
    proceedOne(c) match {
      case Some(ae) =>
        ae match {
          //if return possible anchors:
          case Right(anchors) =>
            trace(s"New anchor(s):${anchors.map(_.node.entity.name)}")
            val forks = anchors.map(TryPath(_)).map(currentPath.pipeAddFork)
            val bottomPath = forks.lastOption
              .getOrElse(throw new AssertionError("Returned anchors should not be empty."))
            recProceed(bottomPath, exceptions)
          //if this path is an end(exception occurred):
          case Left(e) =>
            trace(s"Arg(${c.getCurrentArg}) parse failed with msg:${e.msg}")
            val exceptionAcc = exceptions :+ e
            currentPath.backtrack match {
              //found an unexplored fork:
              case Some(path) =>
                c.restore(path.anchor.contextSnapshot)
                recProceed(path, exceptionAcc)
              //backtrack end:
              case None =>
                if (currentPath.toTop.isComplete) {
                  currentPath
                }
                else { //finally path is not complete, parsing failed:
                  if (exceptionAcc.isEmpty) throw new AssertionError(s"Exception empty.")
                  throw exceptionAcc.maxBy(_.contextSnapshot.argCursor)
                }
            }
        }
      //if complete:
      case None =>
        currentPath.complete //seal this path.
        currentPath.anchor.node match {
          case _: PriorNode =>
            currentPath.trimUpstream
          /*todo: check if the path needs to be filtered to only contain command(and opts?).
           *For now filtering is not a must, prior triggers action before hand.
           */
          case _ =>
            currentPath.toTop.findUnsealedFork match { //if there's another unsealed fork:
              case None => currentPath //all paths are sealed, return
              case Some(path) =>
                c.restore(path.anchor.contextSnapshot)
                recProceed(path, exceptions)
            }
        }
    }
  }

  private def proceedOne(implicit c: Context): Option[AnchorEither] = {
    if (c.isComplete) None
    else {
      val mandatories = c.mandotariesLeft
      if (mandatories.nonEmpty && c.noArgLeft) {
        val more =
          if(mandatories.lengthCompare(1)>0) s" and ${mandatories.size -1} more..." else ""
        Some(ArgParseException(s"More args required for '${mandatories.head.entity.name}'$more", c))
      } else {
        c.nextCateArg.map(_.parsed)
      }
    }
  }
}

private object BacktrackingParser {
  def categorize(arg: String)(implicit argTree: ArgTree): TypedArg[CateArg] = {
    val cateArg: CateArg = arg match {
      case PropertiesExtractor(prop) => prop
      case SingleOptExtractor(sOpt) => SingleOpts(sOpt, arg)
      case LongOptExtractor(lOpt, _, _) => LongOpt(lOpt, arg)
      case paramOrCmd => ParamOrCmd(paramOrCmd)
    }
    TypedArg(cateArg, arg)
  }

  private val SingleOptExtractor: Regex = """-([a-zA-Z].*)""".r
  private val LongOptExtractor: Regex = """-((-[a-zA-Z]\w*)+(=.*)?)""".r
  private object PropertiesExtractor {
    private val RegexExtractor: Regex = """-([A-Z]{1})([a-z]+\w+)\=(.*)""".r
    def unapply(arg: String)(implicit argTree: ArgTree): Option[PropsCate] = arg match {
      case RegexExtractor(flag, k, v) =>
        argTree.props.find(_.entity.flag == flag).map { node =>
          PropsCate(arg, k, v, node)
        }
      case _ => None
    }
  }
}


