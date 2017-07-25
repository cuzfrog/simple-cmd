package com.github.cuzfrog.scmd

object ScmdRouteDSL {
  def cmd(cmd: Command): CmdRoute = new CmdRoute(cmd)

  def param[T](param: Parameter[T]): ValueRoute[T] = DummyValueRoute

  def opt[T](opt: OptionArg[T]): ValueRoute[T] = DummyValueRoute



  implicit class ScmdRouteOps(in: ArgRoute) {
    def ~(that: ArgRoute): ArgRoute = in match {
      case mergeRoute: MergeRoute => mergeRoute.copy(mergeRoute.seq :+ that)
      case other: ArgRoute => MergeRoute(Seq(in, other))
    }
  }

}
