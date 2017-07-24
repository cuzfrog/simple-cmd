package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

sealed trait ScmdRoute {
  private[scmd] val next: Option[ScmdRoute] = None
}

sealed class CmdRoute private[scmd](cmdName: String) extends ScmdRoute {

  def apply(innerRoute: ScmdRoute): ScmdRoute = new CmdRoute(cmdName) {
    override private[scmd] val next = Option(innerRoute)
  }

  def apply(run: => Unit): ScmdRoute = new CmdRoute(cmdName) {
    override private[scmd] val next = Option(new RunRoute[Unit](_ => run))
  }
}

sealed class ValueRoute[+T] private[scmd](valueName: String) extends ScmdRoute {

  private[scmd] val value: Option[T] = None

  def apply[R: ClassTag](innerRoute: Option[T] => R): ScmdRoute = new ValueRoute[T](valueName) {
    val tpe = implicitly[ClassTag[R]]
    override private[scmd] val next = tpe.runtimeClass match {
      case rc if rc == classOf[ScmdRoute] =>Option(innerRoute(value))
      case rc => ???
    }
  }
  def apply(run: Option[T] => Unit): ScmdRoute = new ValueRoute[T](valueName) {
    override private[scmd] val next = Option(new RunRoute(run))
  }
}

final class RunRoute[T] private[scmd](run: Option[T] => Unit) extends ScmdRoute
final case class MergeRoute private[scmd](seq: Seq[ScmdRoute]) extends ScmdRoute

//private object DummyCmdRoute extends CmdRoute("DummyRoute")
private object DummyValueRoute extends ValueRoute[Nothing]("DummyRoute")