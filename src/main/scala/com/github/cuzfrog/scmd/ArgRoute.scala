package com.github.cuzfrog.scmd

sealed trait ArgRoute {
  private[scmd] def next: Option[ArgRoute] = None
  /** */
  def execute: Boolean
}

sealed class CmdRoute private[scmd](cmd: Command,
                                    conditions: Seq[RouteCondition] = Nil) extends ArgRoute {

  def run[R](innerF: => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
    new CmdRoute(cmd, conditions) {
      override private[scmd] val next: Option[ArgRoute] = Option(ev) match {
        case Some(_) => Option(innerF)
        case None => Option(new RunRoute((_: Unit) => innerF, ()))
      }
    }
  override def execute: Boolean = {
    conditions.forall(_.condition == true) && next.forall(_.execute)
  }
}

sealed class SingleValueRoute[+T] private[scmd](value: Option[T]) extends ArgRoute {
  def withValue[R](innerF: Option[T] => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
    new SingleValueRoute[T](value) {
      override private[scmd] val next: Option[ArgRoute] = Option(ev) match {
        case Some(_) => Option(innerF(value))
        case None => Option(new RunRoute(innerF, value))
      }
    }
  override def execute: Boolean = next.forall(_.execute)
}

sealed class VariableValueRoute[+T] private[scmd](value: Seq[T]) extends ArgRoute {
  def withValue[R](innerF: Seq[T] => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
    new VariableValueRoute[T](value) {
      override private[scmd] val next: Option[ArgRoute] = Option(ev) match {
        case Some(_) => Option(innerF(value))
        case None => Option(new RunRoute(innerF, value))
      }
    }
  override def execute: Boolean = next.forall(_.execute)
}

//sealed class MandatoryValueRoute[+T] private[scmd](valueName: String,
//                                                   value: => T) extends ArgRoute {
//
//  def withValue[R](inner: T => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
//    new MandatoryValueRoute[T](valueName, value) {
//      override private[scmd] val next = Option(ev) match {
//        case Some(_) => Option(inner(value))
//        case None => Option(new RunRoute(inner))
//      }
//    }
//}

final class RunRoute[T] private[scmd](runF: (T => R) forSome {type R},
                                      lastValue: T) extends ArgRoute {
  override def execute: Boolean = {
    runF(lastValue)
    true
  }
}
final case class MergeRoute private[scmd](seq: Seq[ArgRoute]) extends ArgRoute {
  override def execute: Boolean = seq.exists(_.execute)
}

sealed class RouteCondition private[scmd](private[scmd] val condition: Boolean)
sealed class RouteConditions private[scmd](private[scmd] val cmd: Command,
                                           private[scmd] val conditions: Seq[RouteCondition])