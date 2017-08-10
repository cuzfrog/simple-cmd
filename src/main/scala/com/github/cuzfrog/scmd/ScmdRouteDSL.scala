package com.github.cuzfrog.scmd

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
      * Link two routes. Linked routes are tried to execute in order.
      * <br><br>
      * If a route's conditions are reached, it's executed, and the whole route ends.
      * Except, a route built by <strong>runThrough</strong> will not end the whole route
      * after execution.
      */
    def ~(that: ArgRoute): ArgRoute = in match {
      case linkRoute: LinkRoute => linkRoute.copy(linkRoute.seq :+ that)
      case otherRoute => LinkRoute(Seq(otherRoute, that))
    }
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
  def run[R](innerF: => R)(implicit ev: R <:< ArgRoute = null): ArgRoute = {
    CmdRoute(rcmd.cmd, rcmd.conditions, rcmd.priorActions).run(innerF, endRoute = true)
  }
  /** Same as run, except it will not end the whole route after running. */
  def runThrough[R](innerF: => R)(implicit ev: R <:< ArgRoute = null): ArgRoute = {
    CmdRoute(rcmd.cmd, rcmd.conditions, rcmd.priorActions).run(innerF, endRoute = false)
  }
}

//todo: add scope to param and opt to limit them to their cmd.