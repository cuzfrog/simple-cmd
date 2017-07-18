package com.github.cuzfrog.scmd.parse

import scala.reflect.ClassTag


private object ArgParser extends TypeAbbr {
  def parse(argTree: ArgTree, args: Array[String]) = {
    new BacktrackingParser(argTree, args)
  }


}


private final class BacktrackingParser(argTree: ArgTree, args: Array[String]) {

  import BacktrackingParser._
  import scala.collection.mutable

  private[this] val context: Context = new Context(argTree, args)
  //private[this] var combinations: mutable.Seq[ValueAnchor] = mutable.Seq.empty[ValueAnchor]


  def parsed = {
    context.nextArg match {
      case Some(firstArg) => consume(firstArg, context)
      case None => throw ArgParseException("No arg found.", context)
    }
  }
}

private object BacktrackingParser extends TypeAbbr {
  private val SingleOptExtractor = """-(\w{1}.*)""".r
  private val LongOptExtractor = """-((-[\w\d]+)+(=.*)?)""".r

  def consume(arg: String,
              context: Context): AnchorEither = {
    arg match {
      case SingleOptExtractor(sOpt) => SingleOpts(sOpt, context).parsed
      case LongOptExtractor(lOpt) => LongOpt(lOpt, context).parsed
      case paramOrCmd => ParamOrCmd(paramOrCmd, context).parsed
    }
  }

  def categorize(arg: String): TypedArg[_] = arg match {
    case SingleOptExtractor(sOpt) => SingleOpts(sOpt,)
    case LongOptExtractor(lOpt) => ClassTag(classOf[LongOpt])
    case paramOrCmd => ClassTag(classOf[ParamOrCmd])
  }

}


