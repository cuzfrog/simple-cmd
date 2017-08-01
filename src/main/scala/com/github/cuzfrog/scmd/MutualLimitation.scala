package com.github.cuzfrog.scmd

import scala.collection.immutable

/**
  * Mutual relationship among value arguments. Defined in tree DSL.
  *
  * @see [[com.github.cuzfrog.scmd.ScmdTreeDefDSL]]
  */
private sealed trait Limitation
private object Limitation {
  def fromOperator(operator: String): Limitation = operator match {
    case "|" => MutuallyExclusive
    case "&" => MutuallyDependent
    case bad => throw new IllegalArgumentException(s"Illegal limitation operator:$bad")
  }
  case object MutuallyExclusive extends Limitation
  case object MutuallyDependent extends Limitation
}

//private sealed trait LimitationGroup[T]
//private final case class MutuallyExclusiveG[T](args: immutable.Seq[T]) extends LimitationGroup[T]
//private final case class MutuallyDependentG[T](args: immutable.Seq[T]) extends LimitationGroup[T]
//
