package com.github.cuzfrog.scmd

import ScmdUtils._

/**
  * Mutual relationship among value arguments. Defined in tree DSL.
  *
  * @see [[com.github.cuzfrog.scmd.ScmdTreeDefDSL]]
  */
sealed trait MutualLimitation
sealed trait MutuallyExclusive extends MutualLimitation
sealed trait MutuallyDependent extends MutualLimitation
//trait used by dsl.

object MutualLimitation {
  def apply(name: scala.Symbol): MutualLimitation = name match {
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
    new Convertible[LimitationTree, List[scala.Symbol]] {
      override def convertTo(a: LimitationTree): List[Symbol] = recTree2seq(a)
    }

  private def recTree2seq(limitationTree: LimitationTree): List[scala.Symbol] = {
    limitationTree match {
      case branch: LimitationBranch =>
        recTree2seq(branch.left) ++ recTree2seq(branch.right)
      case leaf: LimitationLeaf => List(leaf.name)
    }
  }

  implicit class LimitationTreeOps(in: LimitationTree) {
    def findExclusions(name: scala.Symbol): Seq[scala.Symbol] =
      recFindLimitation(in, Nil)(name, Limitation.MExclusive)
    def findDependencies(name: scala.Symbol): Seq[scala.Symbol] =
      recFindLimitation(in, Nil)(name, Limitation.MDependent)
  }
  private def recFindLimitation(tree: LimitationTree,
                                accOppositeSide: Seq[LimitationTree])
                               (implicit name: scala.Symbol,
                                relation: MutualLimitation): Seq[scala.Symbol] = {
    tree match {
      case branch: LimitationBranch =>
        if (branch.relation == relation) {
          val leftProjection = recFindLimitation(branch.left, branch.right +: accOppositeSide)
          val rightProjection = recFindLimitation(branch.right, branch.left +: accOppositeSide)
          leftProjection ++ rightProjection
        } else {
          val leftProjection = recFindLimitation(branch.left, accOppositeSide)
          val rightProjection = recFindLimitation(branch.right, accOppositeSide)
          leftProjection ++ rightProjection
        }
      case leaf: LimitationLeaf =>
        if (leaf.name == name) accOppositeSide.flatMap(_.convertTo[List[scala.Symbol]])
        else Nil
    }
  }
}