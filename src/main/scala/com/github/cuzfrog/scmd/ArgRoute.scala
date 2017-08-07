package com.github.cuzfrog.scmd

sealed trait ArgRoute {
  private[scmd] def next: Option[ArgRoute] = None
  /** */
  def execute: Boolean
}

private sealed class CmdRoute private[scmd](cmd: Command,
                                            conditions: Seq[RouteCondition] = Nil,
                                            priorActions: Seq[(PriorArg, () => Unit)]) extends ArgRoute {

  def run[R](innerF: => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
    new CmdRoute(cmd, conditions, priorActions) {
      override private[scmd] val next: Option[ArgRoute] = Option(ev) match {
        case Some(_) => Option(innerF)
        case None => Option(new RunRoute((_: Unit) => innerF, ()))
      }
    }
  override def execute: Boolean = {
    val actions = priorActions collect {
      case (prior, action) if prior.met.contains(cmd.symbol) => action()
    }
    if (actions.isEmpty)
      conditions.forall(_.condition == true) && next.forall(_.execute)
    else true
  }
}

private final class RunRoute[T] private[scmd](runF: (T => R) forSome {type R},
                                              lastValue: T) extends ArgRoute {
  override def execute: Boolean = {
    runF(lastValue)
    true
  }
}
private final case class MergeRoute private[scmd](seq: Seq[ArgRoute]) extends ArgRoute {
  override def execute: Boolean = seq.exists(_.execute)
}