package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.ArgTree

private object ArgParser {
  def parse(argTree: ArgTree, args: Array[String]) = {

    var cursor: Int = 0

    while (cursor <= args.length) {

    }
  }


}


private final class StateMachine(argTree: ArgTree, args: Seq[String]) {

  import scala.collection.mutable


  private[this] val context: Context = new Context(argTree.toTopNode, args)
  private[this] var combinations: mutable.Seq[ValueAnchor] = mutable.Seq.empty[ValueAnchor]


  /** Not threadsafe, with side-effect. */
  def consume(args: Seq[String]): Seq[String] = {


    args.drop(1)
  }
}

private object StateMachine {
  private val SingleOptExtractor = """-(\w{1}.*)""".r
  private val LongOptExtractor = """-((-[\w\d]+)+(=.*)?)""".r


  private def consumeOneArg(arg: String,
                            context: Context): AnchorEither = {
    arg match {
      case SingleOptExtractor(sOpt) => SingleOpts(sOpt, context).parsed
      case LongOptExtractor(lOpt) =>
      case paramOrCmd =>
    }

    ???
  }

}


