package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.runtime.ScmdRuntime
import com.github.cuzfrog.scmd.runtime.console.ConsoleType

sealed trait ArgRoute {
  private[scmd] def next: Option[ArgRoute] = None
  /** Trigger the route to run. */
  def execute: Boolean
}

private sealed class CmdRoute private[scmd](cmd: Command,
                                            conditions: Seq[RouteCondition] = Nil,
                                            priorActions: Seq[(PriorArg, () => Unit)]) extends ArgRoute {
  /**
    * Add inner func, not really run or execute.
    *
    * @param innerF code to run later, could be an inner route.
    * @param endRoute when inner run finishes, whether to end the route or go through and continue.
    * @param ev to check the innerF is a route or not.
    * @tparam R the return type of inner statement.
    * @return an ArgRoute that encapsulate inner route or inner func.
    */
  def run[R](innerF: => R, endRoute: Boolean)(implicit ev: R <:< ArgRoute = null): ArgRoute =
    new CmdRoute(cmd, conditions, priorActions) {
      override private[scmd] val next: Option[ArgRoute] = Option(ev) match {
        case Some(_) => Option(innerF)
        case None => Option(new RunRoute(() => innerF, endRoute))
      }
    }
  /** Trigger the run func. */
  override def execute: Boolean = {
    val actions = priorActions collect {
      case (prior, action) if prior.met.contains(cmd.symbol) => action()
    }
    if (actions.isEmpty)
      conditions.forall(_.condition == true) && next.forall(_.execute)
    else true
  }
}

private final class RunRoute private[scmd](runF: (() => R) forSome {type R},
                                           endRoute: Boolean) extends ArgRoute {
  override def execute: Boolean = {
    runF()
    endRoute
  }
}
private final case class MergeRoute private[scmd](seq: Seq[ArgRoute]) extends ArgRoute {
  override def execute: Boolean = seq.exists(_.execute)
}