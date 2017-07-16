package com.github.cuzfrog.scmd.parse


private case class SingleOpts(arg: String,
                              nextArg: Option[String], scope: Scope)
private case class LongOpts(arg: String,
                            nextArg: Option[String], scope: Scope)
private case class ParamOrCmd(arg: String, scope: Scope)

private object SingleOpts {
  implicit val parser: Parser[SingleOpts, AnchorEither] = (a: SingleOpts) => {
    val cmdNode = a.scope.cmdNode

    val matchOpt = cmdNode.opts.find(_.entity.abbr.exists(_.head == a.arg.head))
    matchOpt match {
      case Some(optNode) => optNode.tpe.runtimeClass match {
        case rc if rc == classOf[Boolean] => sOpt match {
          case bOpt if bOpt.matches("""\w\=""")
        }
        case _ =>
      }
      case None =>
        Left(ArgParseException(s"Unknow opt[$sOpt]", scope))
    }
  }
}