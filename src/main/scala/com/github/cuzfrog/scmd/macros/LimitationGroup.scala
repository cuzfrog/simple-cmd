package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.ScmdUtils.Convertible
import com.github.cuzfrog.scmd._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.meta._

private case class LimitationGroup(limitation: MutualLimitation, args: immutable.Seq[String])
private object LimitationGroup {

  def fromTuple(tuple: (MutualLimitation, immutable.Seq[TermArg], Int)): LimitationGroup =
    LimitationGroup(tuple._1, tuple._2.map(_.name))

  def fromTree(tree: LimitationTree): immutable.Seq[LimitationGroup] = {
    LimitationUtils.tree2seq(tree).map {
      case (li, args) => LimitationGroup(li, args)
    }
  }

  implicit val definableLimitationTuple: Definable[LimitationGroup] = (a: LimitationGroup) => {
    val symbols =q"""Seq(..${a.args.map(name => Lit.Symbol(scala.Symbol(name)))})"""
    q"(Limitation.${Term.Name(a.limitation.toString)},$symbols)"
  }
}

private sealed trait LimitationTree
private case class LimitationBranch(relation: MutualLimitation,
                                    left: LimitationTree,
                                    right: LimitationTree) extends LimitationTree
private case class LimitationLeaf(name: String) extends LimitationTree

private object LimitationUtils {
  /**
    * Shared function used in both implementation.
    *
    * @param stats   tree DSL statement.
    * @return (MutualLimitation,Seq(termArgs),idx)
    */
  def collectLimitations
  (stats: immutable.Seq[Term.Arg]): immutable.Seq[(LimitationTree, Int)] = {
    stats.zipWithIndex.collect {
      case (stat: Term.ApplyInfix, idx) => (recInfix2Tree(stat), idx)
    }
  }

  private def recInfix2Tree(term: Term.Arg): LimitationTree = {
    term match {
      case stat@Term.ApplyInfix(leftSubs, Term.Name(operator), _, Seq(rightSubs)) =>
        val relationship = Limitation.fromOperator(operator).getOrElse(
          throw new IllegalArgumentException(
            s"Illegal limitation operator:'$operator' before '${rightSubs.syntax}' in '$stat'. " +
              s"This is probably caused by syntax error in tree DSL.")
        )
        LimitationBranch(relationship, recInfix2Tree(leftSubs), recInfix2Tree(rightSubs))
      case Term.Name(argName) => LimitationLeaf(argName)
      case bad =>
        abort(bad.pos,
          s"Bad Tree DSL, unable to parse mutual limitation from statement:${bad.syntax}")
    }
  }

  def tree2seq(limitationTree: LimitationTree)
  : immutable.Seq[(MutualLimitation, immutable.Seq[String])] = {

  }
}

private object LimitationTree {

}