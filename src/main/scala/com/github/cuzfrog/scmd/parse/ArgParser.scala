package com.github.cuzfrog.scmd.parse


private object ArgParser {
  def parse(argTree: ArgTree, args: Array[String]) = {
    val parsedTree = new BacktrackingParser(argTree, args).parsed
  }
}


private final class BacktrackingParser(argTree: ArgTree, args: Array[String]) {

  import BacktrackingParser._
  import scala.collection.mutable

  private[this] implicit val c: Context = new Context(argTree, args.map(categorize).toSeq)
  private[this] var pathCursor: Path = Path(c.anchor(c.getCurrentCmdNode))

  /**
    * Parse args against client defined argTree,
    * return a new tree only containing a path of parsed args.
    */
  def parsed: ArgTree = ???


  private def recProceed(): Anchor[_] = {
    proceedOne() match {
      case Some(ae) =>
        ae match {
          //if return possible anchors:
          case Right(anchors) =>
            val forks = anchors.map(Path).map(pathCursor.pipeAddFork)
            pathCursor = forks.lastOption
              .getOrElse(throw new AssertionError("Returned anchors should not be empty."))
            recProceed()

          //if this path is an end:
          case Left(e) =>
            pathCursor.backtrack match {
              //found a unexplored fork:
              case Some(path) => c.restore(path.anchor.contextSnapshot)

              //cannot backtrack to parent, parsing failed:
              case None => throw e
            }


            ???
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


