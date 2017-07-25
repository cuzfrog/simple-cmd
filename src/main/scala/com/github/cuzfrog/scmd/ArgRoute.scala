package com.github.cuzfrog.scmd

sealed trait ArgRoute {
  private[scmd] val next: Option[ArgRoute] = None
}

sealed class CmdRoute private[scmd](cmd: Command) extends ArgRoute {

  def apply[R](inner: => R)
              (implicit ev: R <:< ArgRoute = null): ArgRoute = new CmdRoute(cmd) {
    override private[scmd] val next = Option(ev) match {
      case Some(_) => Option(inner)
      case None => Option(new RunRoute[Nothing]((_: Option[Nothing]) => inner))
    }
  }
}

sealed class ValueRoute[+T] private[scmd](valueName: String) extends ArgRoute {

  private[scmd] val value: Option[T] = None

  def apply[R](inner: Option[T] => R)
              (implicit ev: R <:< ArgRoute = null): ArgRoute = new ValueRoute[T](valueName) {
    override private[scmd] val next = Option(ev) match {
      case Some(_) => Option(inner(value))
      case None => Option(new RunRoute(inner))
    }
  }
}

final class RunRoute[T] private[scmd](run: (Option[T] => R) forSome {type R}) extends ArgRoute
final case class MergeRoute private[scmd](seq: Seq[ArgRoute]) extends ArgRoute

//private object DummyCmdRoute extends CmdRoute("DummyRoute")
private object DummyValueRoute extends ValueRoute[Nothing]("DummyRoute")