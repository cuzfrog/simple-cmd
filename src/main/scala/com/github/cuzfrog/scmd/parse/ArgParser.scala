package com.github.cuzfrog.scmd.parse

import scala.reflect.ClassTag


private object ArgParser extends TypeAbbr {
  def parse(argTree: ArgTree, args: Array[String]) = {

    new StateMachine(argTree, args)
  }


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

  def typeOfArg(arg: String): ClassTag[_] = arg match {
    case SingleOptExtractor(sOpt) => ClassTag(classOf[SingleOpts])
    case LongOptExtractor(lOpt) => ClassTag(classOf[LongOpt])
    case paramOrCmd => ClassTag(classOf[ParamOrCmd])
  }
}


private final class StateMachine(argTree: ArgTree, args: Array[String]) {

  import StateMachine._
  import scala.collection.mutable

  private[this] val context: Context = new Context(argTree, args)
  //private[this] var combinations: mutable.Seq[ValueAnchor] = mutable.Seq.empty[ValueAnchor]


  def parsed = {
    context.nextArg match {
      case Some(firstArg) => ArgParser.consume(firstArg, context)
      case None => throw ArgParseException("No arg found.", context)
    }
  }
}

private object StateMachine extends TypeAbbr {


}


