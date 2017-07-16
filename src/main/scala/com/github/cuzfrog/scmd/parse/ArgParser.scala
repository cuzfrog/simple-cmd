package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.ArgTree

private object ArgParser {
  def parse(argTree: ArgTree, args: Array[String]) = {

    var cursor: Int = 0

    while (cursor <= args.length) {

    }
  }


}


private final class StateMachine(argTree: ArgTree) {

  import scala.collection.mutable


  private[this] var scope: Scope = Scope(argTree.toTopNode, 0)
  private[this] var combinations: mutable.Seq[ValueAnchor] = mutable.Seq.empty[ValueAnchor]


  /** Not threadsafe, with side-effect. */
  def consume(args: Seq[String]): Seq[String] = {


    args.drop(1)
  }
}

private object StateMachine {
  private val SingleOptExtractor = """-(\w{1}.*)""".r
  private val LongOptExtractor = """-((-[\w\d]+)+(=.*)?)""".r


  private def categorize[A <: ArgCate[A]](arg: String, nextArg: Option[String], scope: Scope): ArgCate[A] = {
    arg match {
      case SingleOptExtractor(sOpt) => SingleOpts(sOpt, nextArg, scope)
      case LongOptExtractor(lOpt) =>
      case paramOrCmd =>

    }
  }


  private def consumeOneArg(arg: String,
                            scope: Scope): Either[ArgParseException, (Scope, Seq[ValueAnchor])] = {
    arg match {
      case SingleOptExtractor(sOpt) => SingleOpts(sOpt, nextArg, scope)

      case LongOptExtractor(lOpt) =>
      case paramOrCmd =>
    }
  }

}


