package com.github.cuzfrog.scmd

object ScmdRouteDSL {
  implicit final class RouteConditionsOps(in: RouteConditions) {
    def run[R](innerF: => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
      new CmdRoute(in.cmd, in.conditions).run(innerF)
  }

  implicit final class CommandOps(cmd: Command) {
    def onConditions(cond1: RouteCondition,
                     condMore: RouteCondition*): RouteConditions =
      new RouteConditions(cmd, cond1 +: condMore)
    def run[R](innerF: => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
      new CmdRoute(cmd).run(innerF)
  }

  implicit final class SingleValueOps[T](a: SingleValue[T]) {
    def expect(compareF: Option[T] => Boolean): RouteCondition =
      new RouteCondition(compareF(a.value))
    def expectEmpty: RouteCondition = new RouteCondition(a.value.isEmpty)
    def expectNonEmpty: RouteCondition = new RouteCondition(a.value.nonEmpty)
  }

  implicit final class SingleValueMandatoryOps[T](a: SingleValue[T] with Mandatory) {
    def expectMandatory(compareF: T => Boolean): RouteCondition =
      new RouteCondition(compareF(a.value.getOrElse(
        throw new IllegalArgumentException(
          s"Mandatory arg: ${a.asInstanceOf[Argument[T]].originalName} of empty value."))))
  }

  implicit final class SingleValueBooleanOps(a: SingleValue[Boolean]) {
    def expectTrue: RouteCondition = new RouteCondition(a.value.contains(true))
    def expectFalse: RouteCondition = new RouteCondition(a.value.contains(false))
  }

  implicit final class PropertyArgOps[T](a: PropertyArg[T] with VariableValue[(String, T)]) {
    def expectByKey(key: String)(compareF: Option[T] => Boolean): RouteCondition = {
      val foundValue = a.value.collect { case (k, v) if k == key => v }.lastOption
      new RouteCondition(compareF(foundValue))
    }

  }

  implicit final class VariableValueOps[T](a: VariableValue[T]) {
    def expect(compareF: Seq[T] => Boolean): RouteCondition = new RouteCondition(compareF(a.value))
  }

  implicit final class ScmdRouteOps(in: ArgRoute) {
    def ~(that: ArgRoute): ArgRoute = in match {
      case mergeRoute: MergeRoute => mergeRoute.copy(mergeRoute.seq :+ that)
      case other: ArgRoute => MergeRoute(Seq(other, that))
    }
  }
}
