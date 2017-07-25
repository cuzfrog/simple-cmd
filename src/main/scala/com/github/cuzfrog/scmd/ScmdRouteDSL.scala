package com.github.cuzfrog.scmd

object ScmdRouteDSL {
  def cmd(cmd: Command): CmdRoute = new CmdRoute(cmd)

  def param[T](param: Parameter[T]): OptionalValueRoute[T] = DummyOptionalValueRoute

  def opt[T](opt: OptionArg[T]): OptionalValueRoute[T] = DummyOptionalValueRoute

  def expectParam[T](opt: Parameter[T]): MandatoryValueRoute[T] = ???

  def expectOpt[T](opt: OptionArg[T]): MandatoryValueRoute[T] = ???

  def expectTrue(opt: OptionArg[Boolean], opts: OptionArg[Boolean]*) = ???

  def expectFalse(opt: OptionArg[Boolean], opts: OptionArg[Boolean]*) = ???

  implicit class ScmdRouteOps(in: ArgRoute) {
    def ~(that: ArgRoute): ArgRoute = in match {
      case mergeRoute: MergeRoute => mergeRoute.copy(mergeRoute.seq :+ that)
      case other: ArgRoute => MergeRoute(Seq(in, other))
    }
  }

}
