package com.github.cuzfrog.scmd

sealed trait ArgRoute {
  private[scmd] def next: Option[ArgRoute] = None
}

sealed class CmdRoute private[scmd](cmd: Command) extends ArgRoute {

  def apply[R](inner: => R)
              (implicit ev: R <:< ArgRoute = null): ArgRoute = new CmdRoute(cmd) {
    override private[scmd] val next = Option(ev) match {
      case Some(_) => Option(inner)
      case None => Option(new RunRoute((_: Unit) => inner))
    }
  }
}

sealed class OptionalValueRoute[+T] private[scmd](valueName: String,
                                                  value: Option[T]) extends ArgRoute {
  def apply[R](inner: Option[T] => R)
              (implicit ev: R <:< ArgRoute = null): ArgRoute =
    new OptionalValueRoute[T](valueName, value) {
      override private[scmd] val next = Option(ev) match {
        case Some(_) => Option(inner(value))
        case None => Option(new RunRoute(inner))
      }
    }
}

sealed class MandatoryValueRoute[+T] private[scmd](valueName: String,
                                                   value: => T) extends ArgRoute {

  def apply[R](inner: T => R)
              (implicit ev: R <:< ArgRoute = null): ArgRoute =
    new MandatoryValueRoute[T](valueName, value) {
      override private[scmd] val next = Option(ev) match {
        case Some(_) => Option(inner(value))
        case None => Option(new RunRoute(inner))
      }
    }
}

final class RunRoute[T] private[scmd](run: (T => R) forSome {type R}) extends ArgRoute
final case class MergeRoute private[scmd](seq: Seq[ArgRoute]) extends ArgRoute

//private object DummyCmdRoute extends CmdRoute("DummyRoute")
private object DummyOptionalValueRoute extends OptionalValueRoute[Nothing]("DummyRoute", None)
private object DummyMandatoryValueRoute extends MandatoryValueRoute[Nothing]("DummyRoute", Empty)