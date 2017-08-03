package com.github.cuzfrog.scmd

/**
  * Mutual relationship among value arguments. Defined in tree DSL.
  *
  * @see [[com.github.cuzfrog.scmd.ScmdTreeDefDSL]]
  */
sealed trait MutualLimitation
sealed trait MutuallyExclusive extends MutualLimitation
sealed trait MutuallyDependent extends MutualLimitation

private object Limitation {
  def fromOperator(operator: String): MutualLimitation = operator match {
    case "|" => MExclusive
    case "&" => MDependent
    case bad => throw new IllegalArgumentException(s"Illegal limitation operator:$bad")
  }

  //defnTerm depends on toString:
  case object MExclusive extends MutuallyExclusive
  case object MDependent extends MutuallyDependent
}

//private sealed trait LimitationGroup[T]
//private final case class MutuallyExclusiveG[T](args: immutable.Seq[T]) extends LimitationGroup[T]
//private final case class MutuallyDependentG[T](args: immutable.Seq[T]) extends LimitationGroup[T]
//
