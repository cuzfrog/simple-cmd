package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd._

import scala.collection.{immutable, mutable}
import scala.meta._
import ScmdUtils._

private object LimitationUtils {
  /**
    * Shared function used in both implementation.
    *
    * @param stats tree DSL statement.
    * @return (LimitationTree,idx)
    */
  def collectLimitationsWithIdx
  (stats: immutable.Seq[Term.Arg]): immutable.Seq[(LimitationTree, Int)] = {
    stats.zipWithIndex.collect {
      case (stat: Term.ApplyInfix, idx) =>
        val tree = recInfix2Tree(stat)
        val duplicates = getDuplicates(tree)
        if (duplicates.nonEmpty)
          abort(stat.pos, s"Duplicates ${duplicates.map(s => s"'${s.name}'").mkString(",")}"
            + s"  found in '${stat.syntax}', this is not allowed." +
            s" If complicated mutual limitation is needed, define them in another clause.")
        (tree, idx)
    }
  }
  def collectLimitations
  (stats: immutable.Seq[Term.Arg]): immutable.Seq[LimitationTree] =
    collectLimitationsWithIdx(stats).map { case (_tree, _) => _tree }

  private def recInfix2Tree(term: Term.Arg): LimitationTree = {
    term match {
      case stat@Term.ApplyInfix(leftSubs, Term.Name(operator), _, Seq(rightSubs)) =>
        val relationship = Limitation.fromOperator(operator).getOrElse(
          throw new IllegalArgumentException(
            s"Illegal limitation operator:'$operator' before '${rightSubs.syntax}' in '$stat'. " +
              s"This is probably caused by syntax error in tree DSL.")
        )
        LimitationBranch(relationship, recInfix2Tree(leftSubs), recInfix2Tree(rightSubs))
      case Term.Name(argName) => LimitationLeaf(scala.Symbol(argName))
      case bad =>
        abort(bad.pos,
          s"Bad Tree DSL, unable to parse mutual limitation from statement:${bad.syntax}")
    }
  }

  private def getDuplicates(limitationTree: LimitationTree): Seq[scala.Symbol] = {
    limitationTree.convertTo[List[scala.Symbol]].groupBy(identity).toSeq.collect {
      case (symbol, seq) if seq.lengthCompare(1) > 0 => symbol
    }
  }

  def tree2seq(limitationTree: LimitationTree): immutable.Seq[String] =
    limitationTree.convertTo[List[scala.Symbol]].map(_.name)

  def getLogicVialations(trees: Seq[LimitationTree]) = {
    
  }

  implicit val definableLimitationTree: Definable[LimitationTree] = {
    case leaf: LimitationLeaf => leaf.defnTerm
    case branch: LimitationBranch => branch.defnTerm
  }

  private implicit val definableLimitationBranch: Definable[LimitationBranch] =
    (a: LimitationBranch) => {
      val name = Lit.Symbol(scala.Symbol(a.relation.toString))
      q"""runtime.buildLimitationBranch(MutualLimitation($name),
                                        ${a.left.defnTerm},
                                        ${a.right.defnTerm})"""
    }

  private implicit val definableLimitationLeaf: Definable[LimitationLeaf] = (a: LimitationLeaf) => {
    q"runtime.buildLimitationLeaf(${Lit.Symbol(a.name)})"
  }
}