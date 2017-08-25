package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.ScmdUtils._
import com.github.cuzfrog.scmd._

import scala.collection.immutable
import scala.meta._

private object LimitationUtils {
  /**
    * Shared function used in both implementation.
    *
    * @param stats tree DSL statement.
    * @return (LimitationTree,idx)
    */
  def collectLimitationsWithIdx
  (stats: immutable.Seq[Term.Arg]): immutable.Seq[(LimitationTree, Int)] = {
    val collected = stats.zipWithIndex.collect {
      case (stat: Term.ApplyInfix, idx) =>
        val tree = recInfix2Tree(stat)

        val duplicates = getDuplicates(tree)
        if (duplicates.nonEmpty)
          abort(stat.pos, s"Duplicates ${duplicates.prettyString}"
            + s"  found in '${stat.syntax}', this is not allowed." +
            s" If complicated mutual limitation is needed, define them in another clause.")
        (tree, idx)
    }

    collected
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

  //Contract: a tree does not have duplicate names.
  def getLogicViolations(trees: Seq[LimitationTree]): Seq[(scala.Symbol, Seq[scala.Symbol])] = {
    val uniquePairs = for {
      (treeX, idxX) <- trees.zipWithIndex
      (treeY, idxY) <- trees.zipWithIndex
      if idxX < idxY
    } yield (treeX, treeY)
    uniquePairs.flatMap {
      case (tree1, tree2) => getLogicViolation(tree1, tree2)
    }
  }
  /** @return List(concern,conflicts) */
  private def getLogicViolation(tree1: LimitationTree,
                                tree2: LimitationTree): Seq[(scala.Symbol, Seq[scala.Symbol])] = {
    val commons = tree1.convertTo[List[scala.Symbol]].intersect(tree2.convertTo[List[scala.Symbol]])
    commons.map { name =>
      val conflicts =
        tree1.findExclusions(name).intersect(tree2.findDependencies(name)) ++
          tree1.findDependencies(name).intersect(tree2.findExclusions(name))
      name -> conflicts
    }.filter { case (name, conflicts) => conflicts.nonEmpty }
  }

  implicit val definableLimitationTree: Definable[LimitationTree] =
    new Definable[LimitationTree] {
      override def defnTerm(a: LimitationTree): Term = a match {
        case leaf: LimitationLeaf => leaf.defnTerm
        case branch: LimitationBranch => branch.defnTerm
      }
    }


  private implicit val definableLimitationBranch: Definable[LimitationBranch] =
    new Definable[LimitationBranch] {
      override def defnTerm(a: LimitationBranch): Term = {
        val name = Lit.Symbol(scala.Symbol(a.relation.toString))
        q"""runtime.buildLimitationBranch(MutualLimitation($name),
                                        ${a.left.defnTerm},
                                        ${a.right.defnTerm})"""
      }
    }

  private implicit val definableLimitationLeaf: Definable[LimitationLeaf] =
    new Definable[LimitationLeaf] {
      override def defnTerm(a: LimitationLeaf): Term = {
        q"runtime.buildLimitationLeaf(${Lit.Symbol(a.name)})"
      }
    }
}