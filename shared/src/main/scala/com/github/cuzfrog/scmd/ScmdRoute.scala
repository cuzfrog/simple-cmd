package com.github.cuzfrog.scmd

sealed trait ScmdRoute {
  private[scmd] val next: Option[ScmdRoute] = None
}

sealed class CmdRoute private(cmdName: String) extends ScmdRoute {

  def apply(innerRoute: ScmdRoute): ScmdRoute = new CmdRoute(cmdName) {
    override private[scmd] val next = Option(innerRoute)
  }

  def apply(run: => Unit): ScmdRoute = new CmdRoute(cmdName) {
    override private[scmd] val next = Option(new RunRoute[Unit](_ => run))
  }
}

sealed class ValueRoute[+T] private(valueName: String) extends ScmdRoute {

  private[scmd] val value: Option[T] = None

  def apply(innerRoute: Option[T] => ScmdRoute): ScmdRoute = new ValueRoute[T](valueName) {
    override private[scmd] val next = Option(innerRoute(value))
  }
  def apply(run: Option[T] => Unit): ScmdRoute = new ValueRoute[T](valueName) {
    override private[scmd] val next = Option(new RunRoute(run))
  }
}

final class RunRoute[T] private[scmd](run: Option[T] => Unit) extends ScmdRoute
final case class MergeRoute private[scmd](seq: Seq[ScmdRoute]) extends ScmdRoute

//private object DummyCmdRoute extends CmdRoute("DummyRoute")
private object DummyValueRoute extends ValueRoute[Nothing]("DummyRoute")