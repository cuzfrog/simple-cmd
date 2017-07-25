package com.github.cuzfrog.scmd

sealed trait ScmdRoute {
  private[scmd] val next: Option[ScmdRoute] = None
}

sealed class CmdRoute private[scmd](cmdName: String) extends ScmdRoute {

  def apply[R](inner: => R)
              (implicit ev: R <:< ScmdRoute = null): ScmdRoute = new CmdRoute(cmdName) {
    override private[scmd] val next = Option(ev) match {
      case Some(_) => Option(inner)
      case None => Option(new RunRoute[Nothing]((_: Option[Nothing]) => inner))
    }
  }
}

sealed class ValueRoute[+T] private[scmd](valueName: String) extends ScmdRoute {

  private[scmd] val value: Option[T] = None

  def apply[R](inner: Option[T] => R)
              (implicit ev: R <:< ScmdRoute = null): ScmdRoute = new ValueRoute[T](valueName) {
    override private[scmd] val next = Option(ev) match {
      case Some(_) => Option(inner(value))
      case None => Option(new RunRoute(inner))
    }
  }
}

final class RunRoute[T] private[scmd](run: (Option[T] => R) forSome {type R}) extends ScmdRoute
final case class MergeRoute private[scmd](seq: Seq[ScmdRoute]) extends ScmdRoute

//private object DummyCmdRoute extends CmdRoute("DummyRoute")
private object DummyValueRoute extends ValueRoute[Nothing]("DummyRoute")