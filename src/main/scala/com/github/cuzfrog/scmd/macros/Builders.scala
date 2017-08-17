package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.macros.logging.TreeBuilderLogging
import com.github.cuzfrog.scmd._
import ScmdUtils._
import com.github.cuzfrog.scmd.macros.LimitationUtils.getLogicViolations

import scala.annotation.tailrec
import scala.collection.immutable
import scala.meta._

private class TreeBuilder {
  /**
    * Build a flat tree from arg definition by source code order.
    *
    * @param argDefs arg definition list from source code with original order.
    * @return A flat TermArgTree with at most one CmdEntryNode (to first level sub-commands).
    */
  @inline
  def buildArgTreeByIdx(appInfo: TermAppInfo,
                        argDefs: immutable.Seq[TermArg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree = {
    val idxDefs = argDefs.toIndexedSeq
    val globalLimitations =
      LimitationUtils.collectLimitations(globalLimitationsStats)

    @tailrec
    def recAdd(builder: IdxTermNodeBuilder, args: immutable.Seq[TermArg]): IdxTermNodeBuilder = {
      if (args.isEmpty) builder
      else recAdd(builder.add(args.head), args.tail)
    }
    val props = idxDefs.collect { case prop: TermProp => prop }
    val priors = idxDefs.collect { case prior: TermPrior => prior }

    val tree = idxDefs.collectFirst { case cmd: TermCmd => cmd } match {
      case None =>
        val params = idxDefs.collect { case param: TermParam => param }
        val opts = idxDefs.collect { case opt: TermOpt => opt }
        TermArgTree(
          appInfo = appInfo,
          topParams = params,
          topOpts = opts,
          priors = priors,
          props = props,
          cmdEntry = TermCommandEntry.placeHolder,
          globalLimitations = globalLimitations)

      case Some(cmd1) =>
        import idxDefs.indexOf
        val topValueArgs = idxDefs.filter(arg => indexOf(arg) < indexOf(cmd1))
        /** param defs above first cmd will be shared by all cmd as their first param. */
        val topParams = topValueArgs.collect { case param: TermParam => param }
        val topOpts = topValueArgs.collect { case opt: TermOpt => opt }

        val tail = idxDefs.filter(arg => indexOf(arg) > indexOf(cmd1))
        val builder = NodeBuilder.newIdxTermBuilder(cmd1, topParams)

        val commands = recAdd(builder, tail).seal
        TermArgTree(
          appInfo = appInfo,
          topParams = Nil,
          topOpts = topOpts,
          priors = priors,
          props = props,
          cmdEntry = TermCommandEntry.createWithCmdNodes(commands),
          globalLimitations = globalLimitations)
    }

    tree $ checkAmbiguousParams $ checkLogicVialation
  }

  /**
    * Build tree from DSL.
    *
    * @see [[com.github.cuzfrog.scmd.macros.NodeBuilder]]
    * @return A TermArgTree shaped by dsl.
    */
  def buildArgTreeByDSL(appInfo: TermAppInfo,
                        argDefs: immutable.Seq[TermArg],
                        dslStats: immutable.Seq[Term.Arg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree = {
    val builder = NodeBuilder.newDslTermBuilder(appInfo, argDefs, dslStats, globalLimitationsStats)

    builder.resolve $ checkAmbiguousParams $ checkLogicVialation
  }

  private def checkAmbiguousParams(termArgTree: TermArgTree): TermArgTree = {
    val ambiguousParams = extractAmbiguousVariableParam(termArgTree)
    if (ambiguousParams.nonEmpty) {
      val head = ambiguousParams.head
      val more =
        if (ambiguousParams.lengthCompare(1) > 0) s" and ${ambiguousParams.size - 1} more..."
        else ""
      abort("Ambiguous args definition. " +
        "Optional/variable parameter cannot be put behind a variable parameter" +
        " within the same command scope." +
        s" '${head._2.name}' is after '${head._1.name}'" + more)
    }
    termArgTree
  }

  /**
    * @param termArgTree the tree to check.
    * @return ambiguous termParams.
    */
  protected def extractAmbiguousVariableParam
  (termArgTree: TermArgTree): Seq[(TermParam, TermParam)] = {
    checkAmbiguity(termArgTree.topParams) ++
      termArgTree.cmdEntry.children.flatMap { cmdNode =>
        recExtractAmbiguousVariableParam(cmdNode)
      }
  }
  private def recExtractAmbiguousVariableParam
  (termCmdNode: TermCmdNode): Seq[(TermParam, TermParam)] = {
    val ambiguousParams = checkAmbiguity(termCmdNode.params)
    termCmdNode.subCmdEntry.children match {
      case Nil => ambiguousParams
      case seq =>
        seq.flatMap(cmdNode => recExtractAmbiguousVariableParam(cmdNode)) ++ ambiguousParams
    }
  }
  private def checkAmbiguity(params: Seq[TermParam]): Seq[(TermParam, TermParam)] = {
    val beginsWithVariable = params.dropWhile(_.isVariable.unary_!)
    if (beginsWithVariable.lengthCompare(2) < 0) return Nil
    val variable = beginsWithVariable.head
    beginsWithVariable.drop(1).filter(p => p.isVariable || !p.isMandatory).map { p =>
      (variable, p)
    }
  }

  private def checkLogicVialation(termArgTree: TermArgTree): TermArgTree = {
    implicit val global: Seq[LimitationTree] = termArgTree.globalLimitations
    checkTreeLogicVialation(termArgTree.topLimitations ++ global)
    termArgTree.cmdEntry.children.foreach(recCheckLogicVialation)
    termArgTree
  }
  private def recCheckLogicVialation(termCmdNode: TermCmdNode)
                                    (implicit globalLimitations: Seq[LimitationTree]): Unit = {
    checkTreeLogicVialation(termCmdNode.limitations ++ globalLimitations)
    termCmdNode.subCmdEntry.children.foreach(recCheckLogicVialation)
  }
  private def checkTreeLogicVialation(trees: Seq[LimitationTree]): Unit = {
    val logicViolations = getLogicViolations(trees)
    if (logicViolations.nonEmpty) {
      val (name, conflicts) = logicViolations.head
      val more =
        if (logicViolations.lengthCompare(1) > 0) s" and ${logicViolations.size - 1} more..."
        else "."
      abort(s"Logic violation found: ${conflicts.prettyString} are defined as " +
        s"both exclusive and dependent against '${name.name}'$more")
    }
  }

  private implicit class Embrace[A](a: A) {
    /** Embrace a function. */
    def $[T](f: A => T): T = f(a)
  }
}

private object TreeBuilder {
  /** Create a builder with logging. */
  def builder: TreeBuilder = new TreeBuilder with TreeBuilderLogging
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
    * @param appInfo                Client defined application info, which contains top command name.
    * @param argDefs                argument defs list that is parsed by macros earlier,
    *                               used to query argument info.
    * @param dslStats               tree def DSL statements collected.
    * @param globalLimitationsStats global mutual limitation statements collected.
    */
  def newDslTermBuilder(appInfo: TermAppInfo,
                        argDefs: immutable.Seq[TermArg],
                        dslStats: immutable.Seq[Term.Arg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): DslTermNodeBuilder =
    new DslTermNodeBuilder(appInfo, argDefs, dslStats, globalLimitationsStats)
}

/** Not thread-safe */
private final class IdxTermNodeBuilder(cmd: TermCmd,
                                       sharedParams: immutable.Seq[TermParam],
                                       lastSibling: Option[IdxTermNodeBuilder]) {

  private[this] var params: immutable.Seq[TermParam] = sharedParams
  private[this] var opts: immutable.Seq[TermOpt] = immutable.Seq.empty
  private val scopeCmdSymbol = Lit.Symbol(scala.Symbol(cmd.name))

  def add(arg: TermArg): IdxTermNodeBuilder = arg match {
    case cmd: TermCmd => new IdxTermNodeBuilder(cmd, sharedParams, Option(this))
    case param: TermParam => this.params :+= param; this
    case opt: TermOpt => this.opts :+= opt; this
    case _: TermPrior => this //ignore priors
    case _: TermProp => this //ignore props
  }

  //non-defensive
  private def build: TermCmdNode = {
    //these CmdNodes are flat at level1 (level0 is the top)
    TermCmdNode(
      cmd,
      params.map(_.copy(parent = scopeCmdSymbol)),
      opts.map(_.copy(parent = scopeCmdSymbol)),
      subCmdEntry = TermCommandEntry.placeHolder)
  }

  def seal: immutable.Seq[TermCmdNode] = lastSibling match {
    case None => immutable.Seq(this.build)
    case Some(last) => last.seal :+ this.build
  }
}

private final class DslTermNodeBuilder(appInfo: TermAppInfo,
                                       argDefs: immutable.Seq[TermArg],
                                       dslStats: immutable.Seq[Term.Arg],
                                       globalLimitationsStats: immutable.Seq[Term.Arg]) {
  def resolve: TermArgTree = {
    val topNode = recResolve(None, dslStats)
    val props = argDefs.collect { case prop: TermProp => prop }
    val priors = argDefs.collect { case prior: TermPrior => prior }
    val globalLimitations = LimitationUtils.collectLimitations(globalLimitationsStats)

    val tree = TermArgTree(
      appInfo = appInfo,
      topParams = topNode.params,
      topOpts = topNode.opts,
      priors = priors,
      props = props,
      cmdEntry = topNode.subCmdEntry,
      topLimitations = topNode.limitations,
      globalLimitations = globalLimitations
    )

    val argDifference = argDefs.map(_.name)
      .diff(tree.convertTo[immutable.Seq[TermArg]].map(_.name))
    if (argDifference.nonEmpty)
      abort(s"Arg not defined in tree dsl: ${argDifference.mkString(",")}." +
        s" Comment these argDefs out or put them in the tree.")

    val descentDuplicates = getDescentDuplicates(tree)
    if (descentDuplicates.nonEmpty)
      abort(descentDuplicates.head.pos,
        s"Arg cannot duplicate along lineage: ${descentDuplicates.map(_.name).mkString(",")}")

    tree
  }

  private def recResolve(termCmdOpt: Option[TermCmd],
                         dslStats: immutable.Seq[Term.Arg]): TermCmdNode = {
    var isCmdEntryOptional = false
    val strippedStats = dslStats.map {
      case Term.Select(cmdTerm, Term.Name("?")) =>
        isCmdEntryOptional = true
        cmdTerm //strip off "?"
      case bad: Term.Select =>
        abort(bad.pos, s"Only use val name in tree DSL, ${bad.syntax} is not supported.")
      case other => other
    }

    val singleArgs: immutable.Seq[(TermArg, Int)] = strippedStats.zipWithIndex.collect {
      case (Term.Name(argName), idx) => queryArg(argName) match {
        case Some(termArg) => termArg -> idx
        case None => notDefined(argName)
      }
    }

    val limitationTreeWithIdx = LimitationUtils.collectLimitationsWithIdx(dslStats)
    val groupArgs: immutable.Seq[(immutable.Seq[TermArg], Int)] = limitationTreeWithIdx
      .map { case (tree, idx) =>
        val args = LimitationUtils.tree2seq(tree).map { argName =>
          queryArg(argName) match {
            case Some(termArg) => termArg
            case None => notDefined(argName)
          }
        }
        (args, idx)
      }

    val termArgs =
      (singleArgs.map { case (termArg, idx) => immutable.Seq(termArg) -> idx } ++ groupArgs)
        .sortBy(_._2).flatMap { case (termArg, _) => termArg }

    val duplicates = termArgs.groupBy(identity)
      .collect { case (termArg, seq) if seq.size > 1 => termArg }
    if (duplicates.nonEmpty)
      abort(dslStats.head.pos,
        s"Duplicates of arguments:${duplicates.map(_.name).mkString(",")}" +
          s" at the same place in tree definition.")

    val illegalArgs = termArgs.collect {
      case illegal@(_: TermPrior | _: TermProp) => illegal
    }
    if (illegalArgs.nonEmpty)
      abort(illegalArgs.head.pos,
        s"Prior arg and properties are global, and cannot be define in tree:" +
          s" ${illegalArgs.map(_.name).mkString(",")}" +
          s" They are automatically picked up once defined.")

    val subCmdEntry = {
      val childCompoundCmds: immutable.Seq[(TermCmdNode, Int)] =
        strippedStats.zipWithIndex.collect {
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

      val cmdEntryTerm =
        TermCommandEntry.getTerm(sortedChildCmdNodes.nonEmpty && !isCmdEntryOptional)
      TermCommandEntry(cmdEntryTerm, sortedChildCmdNodes)
    }

    val scopeCmdSymbol = Lit.Symbol(scala.Symbol(termCmdOpt.map(_.name)
      .getOrElse(appInfo.appInfo.name)))
    TermCmdNode(
      cmd = termCmdOpt.getOrElse(TermCmd.dummy),
      params = termArgs.collect { case a: TermParam => a.copy(parent = scopeCmdSymbol) },
      opts = termArgs.collect { case a: TermOpt => a.copy(parent = scopeCmdSymbol) },
      subCmdEntry = subCmdEntry,
      limitations = limitationTreeWithIdx.map { case (_tree, _) => _tree }
    )
  }

  /** Return termArgs that duplicate along lineage. */
  private def getDescentDuplicates(tree: TermArgTree): immutable.Seq[TermArg] = {
    val topAncestors = //priors and properties are forbidden during building.
      tree.topParams ++ tree.topOpts

    def recGetDescentDuplicates(ancesters: immutable.Seq[TermArg],
                                cmdNode: TermCmdNode,
                                acc: immutable.Seq[TermArg]): immutable.Seq[TermArg] = {
      val descendants = cmdNode.params ++ cmdNode.opts :+ cmdNode.cmd
      def duplicates = ancesters.intersect(descendants)
      cmdNode.subCmdEntry.children match {
        case Nil => acc ++ duplicates
        case subCmds => subCmds.flatMap { n =>
          recGetDescentDuplicates(ancesters ++ descendants, n, acc ++ duplicates)
        }
      }
    }
    tree.cmdEntry.children.flatMap { n =>
      recGetDescentDuplicates(topAncestors, n, Nil)
    }
  }

  private def queryArg(name: String): Option[TermArg] = argDefs.find(_.name == name)

  private def notDefined(name: String): Nothing =
    throw new IllegalArgumentException(s"Arg in Tree DSL not defined yet: $name")
}