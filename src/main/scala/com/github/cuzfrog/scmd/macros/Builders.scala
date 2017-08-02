package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.Limitation
import com.github.cuzfrog.scmd.macros.logging.TreeBuilderLogging

import scala.annotation.tailrec
import scala.collection.immutable
import scala.meta._

private trait TreeBuilder {
  /**
    * Build a flat tree from arg definition by source code order.
    *
    * @param argDefs arg definition list from source code with original order.
    * @return A flat TermArgTree with at most one CmdEntryNode (to first level sub-commands).
    */
  @inline
  def buildArgTreeByIdx(argDefs: immutable.Seq[TermArg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree

  /**
    * Build tree from DSL.
    *
    * @see [[com.github.cuzfrog.scmd.macros.NodeBuilder]]
    * @return A TermArgTree shaped by dsl.
    */
  def buildArgTreeByDSL(argDefs: immutable.Seq[TermArg],
                        dslStats: immutable.Seq[Term.Arg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree
}

private object TreeBuilder {
  /** Create a builder with logging. */
  def builder: TreeBuilder = new TreeBuilderImpl with TreeBuilderLogging

  private class TreeBuilderImpl extends TreeBuilder {
    /**
      * Build a flat tree from arg definition by source code order.
      *
      * @param argDefs arg definition list from source code with original order.
      * @return A flat TermArgTree with at most one CmdEntryNode (to first level sub-commands).
      */
    @inline
    def buildArgTreeByIdx(argDefs: immutable.Seq[TermArg],
                          globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree = {
      val idxDefs = argDefs.toIndexedSeq
      val globalLimitations = NodeBuilder.collectLimitations(argDefs, globalLimitationsStats)
        .map { case (l, s, _) => l -> s.map(_.name) }

      @tailrec
      def recAdd(builder: IdxTermNodeBuilder, args: immutable.Seq[TermArg]): IdxTermNodeBuilder = {
        if (args.isEmpty) builder
        else recAdd(builder.add(args.head), args.tail)
      }

      idxDefs.collectFirst { case cmd: TermCmd => cmd } match {
        case None =>
          val params = idxDefs.collect { case param: TermParam => param }
          val opts = idxDefs.collect { case opt: TermOpt => opt }
          TermArgTree(params, opts, TermCommandEntry.placeHolder,
            globalLimitations = globalLimitations)

        case Some(cmd1) =>
          import idxDefs.indexOf
          val topLevelValueArgs = idxDefs.filter(arg => indexOf(arg) < indexOf(cmd1))

          val topLevelOpts = topLevelValueArgs.collect { case opt: TermOpt => opt }
          /** param defs above first cmd will be shared by all cmd as their first param. */
          val topLevelParam = topLevelValueArgs.collect { case param: TermParam => param }

          val tail = idxDefs.filter(arg => indexOf(arg) > indexOf(cmd1))
          val builder = NodeBuilder.newIdxTermBuilder(cmd1, topLevelParam)

          val commands = recAdd(builder, tail).seal
          TermArgTree(Nil, topLevelOpts,
            TermCommandEntry.createWithCmdNodes(commands),
            globalLimitations = globalLimitations)
      }
    }

    /**
      * Build tree from DSL.
      *
      * @see [[com.github.cuzfrog.scmd.macros.NodeBuilder]]
      * @return A TermArgTree shaped by dsl.
      */
    def buildArgTreeByDSL(argDefs: immutable.Seq[TermArg],
                          dslStats: immutable.Seq[Term.Arg],
                          globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree = {

      val builder = NodeBuilder.newDslTermBuilder(argDefs, dslStats, globalLimitationsStats)

      builder.resolve
    }
  }
}

private object NodeBuilder {
  /**
    * Create a new index term builder.
    *
    * @param cmd1         the first cmd occurred in argument defs.
    * @param sharedParams the params defined above the first cmd,
    *                     they are meant to be shared by all cmd as prior params.
    */
  def newIdxTermBuilder(cmd1: TermCmd,
                        sharedParams: immutable.Seq[TermParam]): IdxTermNodeBuilder = {
    new IdxTermNodeBuilder(cmd1, sharedParams, None)
  }

  /**
    * Create a new dsl builder.
    *
    * @param argDefs                argument defs list that is parsed by macros earlier,
    *                               used to query argument info.
    * @param dslStats               tree def DSL statements collected.
    * @param globalLimitationsStats global mutual limitation statements collected.
    */
  def newDslTermBuilder(argDefs: immutable.Seq[TermArg],
                        dslStats: immutable.Seq[Term.Arg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): DslTermNodeBuilder =
    new DslTermNodeBuilder(argDefs, dslStats, globalLimitationsStats)


  /** Shared function used in both implementation. */
  def collectLimitations
  (argDefs: immutable.Seq[TermArg],
   stats: immutable.Seq[Term.Arg]): immutable.Seq[(Limitation, immutable.Seq[TermArg], Int)] = {
    @tailrec
    def recInfix2seq(subTerm: Term,
                     acc: immutable.Seq[String] = immutable.Seq()): immutable.Seq[String] =
      subTerm match {
        case Term.Name(argName) => argName +: acc
        case Term.ApplyInfix(subs, Term.Name(operator), _, Seq(Term.Name(argName)))
          if operator == "&" || operator == "|" => recInfix2seq(subs, argName +: acc)
        case bad => throw new IllegalArgumentException(s"Tree DSL cannot be parsed: $bad")
      }

    def queryArg(name: String): Option[TermArg] = argDefs.find(_.name == name)

    stats.zipWithIndex.collect {
      case (Term.ApplyInfix(subs, Term.Name(operator), _, Seq(Term.Name(lastArgName))), idx) =>
        val relationSeq = (recInfix2seq(subs) :+ lastArgName)
          .map(argName => queryArg(argName)
            .getOrElse(
              throw new IllegalArgumentException(s"Arg in Tree DSL not defined yet:$argName")))
        val limitation = Limitation.fromOperator(operator)
        (limitation, relationSeq, idx)
    }
  }
}

/** Not thread-safe */
private final class IdxTermNodeBuilder(cmd: TermCmd,
                                       sharedParams: immutable.Seq[TermParam],
                                       lastSibling: Option[IdxTermNodeBuilder]) {

  private[this] var params: immutable.Seq[TermParam] = sharedParams
  private[this] var opts: immutable.Seq[TermOpt] = immutable.Seq.empty

  def add(arg: TermArg): IdxTermNodeBuilder = arg match {
    case cmd: TermCmd => new IdxTermNodeBuilder(cmd, sharedParams, Option(this))
    case param: TermParam => this.params :+= param; this
    case opt: TermOpt => this.opts :+= opt; this
  }

  //non-defensive
  private def build: TermCmdNode = {
    //these CmdNodes are flat at level1 (level0 is the top)
    TermCmdNode(cmd, params, opts, subCmdEntry = TermCommandEntry.placeHolder)
  }

  def seal: immutable.Seq[TermCmdNode] = lastSibling match {
    case None => immutable.Seq(this.build)
    case Some(last) => last.seal :+ this.build
  }
}

private final class DslTermNodeBuilder(argDefs: immutable.Seq[TermArg],
                                       dslStats: immutable.Seq[Term.Arg],
                                       globalLimitationsStats: immutable.Seq[Term.Arg]) {

  def resolve: TermArgTree = {
    val topNode = recResolve(None, dslStats)

    val gobalLimitations = collectLimitations(globalLimitationsStats)
      .map { case (l, s, _) => l -> s.map(_.name) }

    TermArgTree(
      topParams = topNode.params,
      topOpts = topNode.opts,
      cmdEntry = topNode.subCmdEntry,
      topLimitations = topNode.limitations,
      globalLimitations = gobalLimitations
    )
  }

  private def recResolve(termCmdOpt: Option[TermCmd],
                         dslStats: immutable.Seq[Term.Arg]): TermCmdNode = {
    val singleArgs: immutable.Seq[(TermArg, Int)] = dslStats.zipWithIndex.collect {
      case (Term.Name(argName), idx) => queryArg(argName) match {
        case Some(termArg) => termArg -> idx
        case None => notDefined(argName)
      }
    }
    val groupArgs: immutable.Seq[(Limitation, immutable.Seq[TermArg], Int)] =
      collectLimitations(dslStats)

    val termArgs =
      (singleArgs.map { case (termArg, idx) => immutable.Seq(termArg) -> idx } ++
        groupArgs.map { case (_, relationSeq, idx) => relationSeq -> idx })
        .sortBy(_._2).flatMap { case (termArg, _) => termArg }

    val duplicates = termArgs.groupBy(t => t)
      .collect { case (termArg, seq) if seq.size > 1 => termArg }
    if (duplicates.nonEmpty)
      abort(dslStats.head.pos,
        s"Duplicates of arguments:${duplicates.mkString(",")} in tree definition.")

    val subCmdEntry = {
      val childCompoundCmds = dslStats.zipWithIndex.collect {
        case (q"$cmd(..$subParams)", idx) =>
          val cmdName = cmd.syntax
          val termCmd = queryArg(cmdName) match {
            case Some(tc: TermCmd) => tc
            case _ => notDefined(cmdName)
          }
          (recResolve(Some(termCmd), subParams), idx)
      }
      val childSingleCmds = singleArgs.collect { case (cmd: TermCmd, idx) =>
        (recResolve(Some(cmd), Nil), idx)
      }
      val sortedChildCmdNodes: immutable.Seq[TermCmdNode] =
        (childCompoundCmds ++ childSingleCmds).sortBy(_._2).map(_._1)

      val cmdEntryTerm = TermCommandEntry.getTerm(sortedChildCmdNodes.nonEmpty)
      //todo:implement optional cmd entry.
      TermCommandEntry(cmdEntryTerm, sortedChildCmdNodes)
    }

    TermCmdNode(
      cmd = termCmdOpt.getOrElse(TermCmd.dummy),
      params = termArgs.collect { case a: TermParam => a },
      opts = termArgs.collect { case a: TermOpt => a },
      subCmdEntry = subCmdEntry,
      limitations = groupArgs.map { case (limitation, relationSeq, _) =>
        limitation -> relationSeq.map(_.name)
      }
    )
  }

  private def collectLimitations
  (stats: immutable.Seq[Term.Arg]): immutable.Seq[(Limitation, immutable.Seq[TermArg], Int)] =
    NodeBuilder.collectLimitations(argDefs, stats)

  private def queryArg(name: String): Option[TermArg] = argDefs.find(_.name == name)

  private def notDefined(name: String): Nothing =
    throw new IllegalArgumentException(s"Arg in Tree DSL not defined yet:$name")
}