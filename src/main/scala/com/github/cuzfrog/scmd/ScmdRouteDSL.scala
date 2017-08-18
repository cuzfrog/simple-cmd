package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

/**
  * DSL to built scmd route.
  */
object ScmdRouteDSL {
  /**
    * Application(top command) entry.
    */
  final def app(implicit appInfo: AppInfo): RouteCommand = Command.topCmd(appInfo.name)

  implicit final class CommandOps(cmd: Command) extends RouteCommandOperations {
    protected val rcmd: RouteCommand = cmd
  }

  implicit final class RouteCommandOps(protected val rcmd: RouteCommand)
    extends RouteCommandOperations

  implicit final class RouteConditionOps(in: RouteCondition) {
    def ||(that: RouteCondition): RouteCondition =
      new RouteCondition(in.condition || that.condition)
    def &&(that: RouteCondition): RouteCondition =
      new RouteCondition(in.condition && that.condition)
  }

  implicit final class SingleValueOps[T](a: SingleValue[T]) {
    def expect(compareF: Option[T] => Boolean): RouteCondition =
      new RouteCondition(compareF(a.v))
    def expectEmpty: RouteCondition = new RouteCondition(a.v.isEmpty)
    def expectNonEmpty: RouteCondition = new RouteCondition(a.v.nonEmpty)
  }

  implicit final class SingleValueMandatoryOps[T](a: SingleValue[T] with Mandatory) {
    def expectMandatorily(compareF: T => Boolean): RouteCondition =
      new RouteCondition(compareF(a.v.getOrElse(
        throw new IllegalArgumentException(
          s"Mandatory arg: ${a.asInstanceOf[Argument[T]].originalName} of empty value."))))
  }

  implicit final class SingleValueBooleanOps(a: SingleValue[Boolean]) {
    def expectTrue: RouteCondition = new RouteCondition(a.v.contains(true))
    def expectFalse: RouteCondition = new RouteCondition(a.v.contains(false))
  }

  implicit final class PropertyArgOps[T](a: PropertyArg[T] with VariableValue[(String, T)]) {
    def expectByKey(key: String)(compareF: Option[T] => Boolean): RouteCondition = {
      val foundValue = a.v.collect { case (k, v) if k == key => v }.lastOption
      new RouteCondition(compareF(foundValue))
    }
  }

  implicit final class VariableValueOps[T](a: VariableValue[T]) {
    def expect(compareF: Seq[T] => Boolean): RouteCondition = new RouteCondition(compareF(a.v))
  }

  implicit final class ScmdRouteOps(in: ArgRoute) {
    /**
      * Link two routes in logic 'or' pattern. Linked routes are tried to execute in order.
      * If one route's conditions are reached, it's executed, and the whole route ends.
      */
    def ~(that: ArgRoute): ArgRoute = LinkRoute(Seq(in, that), exhaustive = false)

    /** Link two routes in logic 'and' pattern. Linked routes are tried to execute in order.
      * Both routes' conditions will be tested, and those with reached conditions will be executed.
      * If more than zero of the routes have been executed, the whole route will end.
      */
    def &(that: ArgRoute): ArgRoute = LinkRoute(Seq(in, that), exhaustive = true)
  }
}

sealed case
class RouteCommand private[scmd](private[scmd] val cmd: Command,
                                 private[scmd] val priorActions: Seq[(PriorArg, () => Unit)],
                                 private[scmd] val conditions: Seq[RouteCondition])
private object RouteCommand {
  implicit def fromCommand(cmd: Command): RouteCommand = RouteCommand(cmd, Nil, Nil)
}
sealed class RouteCondition private[scmd](private[scmd] val condition: Boolean)

/** Use shared trait instead of chained implicit conversion to better adapt IDE. */
sealed trait RouteCommandOperations {
  protected def rcmd: RouteCommand

  def onConditions(cond1: RouteCondition,
                   condMore: RouteCondition*): RouteCommand = {
    rcmd.copy(conditions = rcmd.conditions ++ (cond1 +: condMore))
  }
  def runOnPrior(a: PriorArg)(action: => Unit): RouteCommand = {
    rcmd.copy(priorActions = rcmd.priorActions :+ (a -> (() => action)))
  }
  //@formatter:off
  /**
    * Define a run clause. Which will be executed when def-class `runWithRoute`.<br><br>
    *
    * @see [[com.github.cuzfrog.scmd.ScmdRouteRunTypeConstraint]]
    * @param innerF statement.
    * @tparam R the return type of statement.<br>
    *           `RouteCommand` is constrained by ambiguous implicits.<br><br>
    *           Caution: Inner route can only be executed when the route returns.
    *           If return type is Unit, the route will be ignored. e.g.:{{{
    * cmd.run{
    *   subCmd.run{...} //not returned, so ignored.
    *   println("do other things")
    * }
    * }}}
    * @return an `ArgRoute`
    */
  //@formatter:on
  def run[R: ClassTag : ScmdRouteRunTypeConstraint](innerF: => R): ArgRoute = {
    CmdRoute(rcmd.cmd, rcmd.conditions, rcmd.priorActions).run(innerF)
  }

  //todo: provide a macro api to check routes.
}

/**
  * Constrain `RouteCommand` not to return in run clause.
  */
trait ScmdRouteRunTypeConstraint[R]
private object ScmdRouteRunTypeConstraint extends LowLevelImplicitsForScmdRouteRunTypeConstraint {
  implicit def routeCommandAmbiguous1: ScmdRouteRunTypeConstraint[RouteCommand] =
    new ScmdRouteRunTypeConstraint[RouteCommand] {}
  implicit def routeCommandAmbiguous2: ScmdRouteRunTypeConstraint[RouteCommand] =
    new ScmdRouteRunTypeConstraint[RouteCommand] {}
}
private sealed trait LowLevelImplicitsForScmdRouteRunTypeConstraint {
  implicit def default[R]: ScmdRouteRunTypeConstraint[R] = new ScmdRouteRunTypeConstraint[R] {}
}

//todo: add scope to param and opt to limit them to their cmd. (why necessary?)