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
  def fromOperator(operator: String): Option[MutualLimitation] = operator match {
    case "|" => Some(MExclusive)
    case "&" => Some(MDependent)
    case _ => None
  }

  //defnTerm depends on toString:
  case object MExclusive extends MutuallyExclusive
  case object MDependent extends MutuallyDependent
}

private sealed trait LimitationTree extends Product with Serializable
private final case class LimitationBranch(relation: MutualLimitation,
                                    left: LimitationTree,
                                    right: LimitationTree) extends LimitationTree
private final case class LimitationLeaf(name: scala.Symbol) extends LimitationTree