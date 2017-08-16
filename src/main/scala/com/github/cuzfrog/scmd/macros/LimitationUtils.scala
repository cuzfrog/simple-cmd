package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd._

import scala.collection.immutable
import scala.meta._

private object LimitationUtils {
  /**
    * Shared function used in both implementation.
    *
    * @param stats   tree DSL statement.
    * @return (LimitationTree,idx)
    */
  def collectLimitationsWithIdx
  (stats: immutable.Seq[Term.Arg]): immutable.Seq[(LimitationTree, Int)] = {
    stats.zipWithIndex.collect {
      case (stat: Term.ApplyInfix, idx) => (recInfix2Tree(stat), idx)
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

  def tree2seq(limitationTree: LimitationTree): immutable.Seq[String] = {
    ???
  }

  implicit val definableLimitationTree: Definable[LimitationTree] = ???

  private implicit val definableLimitationBranch: Definable[LimitationBranch] = ???

  private implicit val definableLimitationLeaf: Definable[LimitationLeaf] = (a: LimitationLeaf) =>{
    q"LimitationLeaf"
  }
}