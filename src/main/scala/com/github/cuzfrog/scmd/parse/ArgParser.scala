package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.ArgTree

private object ArgParser {
  def parse(argTree: ArgTree, args: Array[String]) = {

    new StateMachine(argTree, args)
  }
}


private final class StateMachine(argTree: ArgTree, args: Array[String]) {

  import StateMachine._
  import scala.collection.mutable

  private[this] val context: Context = new Context(argTree, args)
  private[this] var combinations: mutable.Seq[ValueAnchor] = mutable.Seq.empty[ValueAnchor]


  def parsed = {
    context.nextArg match {
      case Some(firstArg) => consume(firstArg, context)
      case None => throw ArgParseException("No arg found.", context)
    }
  }
}

private object StateMachine extends TypeAbbr {
  private val SingleOptExtractor = """-(\w{1}.*)""".r
  private val LongOptExtractor = """-((-[\w\d]+)+(=.*)?)""".r


  private def consume(arg: String,
                      context: Context): AnchorEither = {
    arg match {
      case SingleOptExtractor(sOpt) => SingleOpts(sOpt, context).parsed
      case LongOptExtractor(lOpt) => LongOpt(lOpt, context).parsed
      case paramOrCmd => ???
    }
  }

}


