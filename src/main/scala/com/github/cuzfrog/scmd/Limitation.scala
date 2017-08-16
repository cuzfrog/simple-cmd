package com.github.cuzfrog.scmd

/**
  * Mutual relationship among value arguments. Defined in tree DSL.
  *
  * @see [[com.github.cuzfrog.scmd.ScmdTreeDefDSL]]
  */
sealed trait MutualLimitation
case object MutuallyExclusive extends MutualLimitation
case object MutuallyDependent extends MutualLimitation

private object Limitation {
  def fromOperator(operator: String): Option[MutualLimitation] = operator match {
    case "|" => Some(MutuallyExclusive)
    case "&" => Some(MutuallyDependent)
    case _ => None
  }

  //defnTerm depends on toString:
  type MExclusive = MutuallyExclusive.type
  type MDependent = MutuallyDependent.type
}

private sealed trait LimitationTree extends Product with Serializable
private final case class LimitationBranch(relation: MutualLimitation,
                                    left: LimitationTree,
                                    right: LimitationTree) extends LimitationTree
private final case class LimitationLeaf(name: scala.Symbol) extends LimitationTree