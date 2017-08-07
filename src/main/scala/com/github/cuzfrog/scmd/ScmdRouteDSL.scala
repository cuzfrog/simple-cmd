package com.github.cuzfrog.scmd

object ScmdRouteDSL {
  implicit final def cmd2CmdRoute(cmd: Command): CmdRoute = new CmdRoute(cmd)

  implicit final class CommandOps(cmd: Command) {
    def onConditions(cond1: RouteCondition,
                     condMore: RouteCondition*): CmdRoute = new CmdRoute(cmd, cond1 +: condMore)
  }

  implicit class SingleValueOps[T](a: SingleValue[T]) {
    def expect(compareF: Option[T] => Boolean): RouteCondition =
      new RouteCondition(compareF(a.value))
    def withValue[R](innerF: Option[T] => R): ArgRoute =
      new SingleValueRoute[T](a.value).withValue(innerF)
  }

  implicit class SingleValueBooleanOps(a: SingleValue[Boolean]) {
    def expectTrue: RouteCondition = new RouteCondition(a.value.contains(true))
    def expectFalse: RouteCondition = new RouteCondition(a.value.contains(false))
  }

  implicit class VariableValueOps[T](a: VariableValue[T]) {
    def expect(compareF: Seq[T] => Boolean): RouteCondition = new RouteCondition(compareF(a.value))
    def withValue[R](innerF: Seq[T] => R): ArgRoute =
      new VariableValueRoute[T](a.value).withValue(innerF)
  }

  implicit class ScmdRouteOps(in: ArgRoute) {
    def ~(that: ArgRoute): ArgRoute = in match {
      case mergeRoute: MergeRoute => mergeRoute.copy(mergeRoute.seq :+ that)
      case other: ArgRoute => MergeRoute(Seq(other, that))
    }
  }

}
