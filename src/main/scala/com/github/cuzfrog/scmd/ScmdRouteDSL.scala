package com.github.cuzfrog.scmd

object ScmdRouteDSL {
  def cmd(cmd: String): CmdRoute = new CmdRoute(cmd)

  def param[T](param: Option[T]): ValueRoute[T] = DummyValueRoute

  def opt[T](opt: Option[T]): ValueRoute[T] = DummyValueRoute

  implicit class ScmdRouteOps(in: ScmdRoute) {
    def ~(that: ScmdRoute): ScmdRoute = in match {
      case mergeRoute: MergeRoute => mergeRoute.copy(mergeRoute.seq :+ that)
      case other: ScmdRoute => MergeRoute(Seq(in, other))
    }
  }

}
