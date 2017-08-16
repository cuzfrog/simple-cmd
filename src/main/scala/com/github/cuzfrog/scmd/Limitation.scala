package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.ScmdUtils.Convertible

/**
  * Mutual relationship among value arguments. Defined in tree DSL.
  *
  * @see [[com.github.cuzfrog.scmd.ScmdTreeDefDSL]]
  */
sealed trait MutualLimitation
sealed trait MutuallyExclusive extends MutualLimitation
sealed trait MutuallyDependent extends MutualLimitation
//trait used by dsl.

object MutualLimitation{
  def apply(name:scala.Symbol):MutualLimitation = name match{
    case 'MExclusive => Limitation.MExclusive
    case 'MDependent => Limitation.MDependent
  }
}

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

private object LimitationTree {
  implicit def convert2seqOfSymbol: Convertible[LimitationTree, List[scala.Symbol]] =
    (a: LimitationTree) => recTree2seq(a)

  private def recTree2seq(limitationTree: LimitationTree): List[scala.Symbol] = {
    limitationTree match {
      case branch: LimitationBranch =>
        recTree2seq(branch.left) ++ recTree2seq(branch.right)
      case leaf: LimitationLeaf => List(leaf.name)
    }
  }
}