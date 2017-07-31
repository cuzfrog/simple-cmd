package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.Argument

import scala.annotation.tailrec
import scala.collection.immutable
import scala.meta._

private object TreeBuilder {

  /**
    * Build a flat tree from arg definition by source code order.
    *
    * @param argDefs arg definition list from source code with original order.
    * @return A flat TermArgTree with at most one CmdEntryNode (to first level sub-commands).
    */
  @inline
  def buildArgTreeByIdx(argDefs: immutable.Seq[TermArg]): TermArgTree = {
    val idxDefs = argDefs.toIndexedSeq

    @tailrec
    def recAdd(builder: IdxTermNodeBuilder, args: immutable.Seq[TermArg]): IdxTermNodeBuilder = {
      if (args.isEmpty) builder
      else recAdd(builder.add(args.head), args.tail)
    }

    idxDefs.collectFirst { case cmd: TermCmd => cmd } match {
      case None =>
        val params = idxDefs.collect { case param: TermParam => param }
        val opts = idxDefs.collect { case opt: TermOpt => opt }
        TermArgTree(params, opts, TermCommandEntry.default)
      case Some(cmd1) =>
        import idxDefs.indexOf
        val topLevelOpts = idxDefs.filter(arg => indexOf(arg) < indexOf(cmd1)).collect {
          case opt: TermOpt => opt
          case param: TermParam =>
            //todo: top level param before commands be parsed as common param of each command.
            abort(param.pos, s"Parameter[${param.term.syntax}] cannot be defined before first command.")
        }
        val tail = idxDefs.filter(arg => indexOf(arg) > indexOf(cmd1))
        val builder = NodeBuilder.newIdxTermBuilder(cmd1)

        val commands = recAdd(builder, tail).seal
        TermArgTree(Nil, topLevelOpts, TermCommandEntry.defaultWithCmdNodes(commands))
    }
  }

  /**
    * Build tree from DSL.
    *
    * @return A TermArgTree shaped by dsl.
    */
  def buildArgTreeByDSL(argDefs: immutable.Seq[TermArg],
                        dslParams: immutable.Seq[Term.Arg]): TermArgTree = {

    val builder = new DslTermNodeBuilder(argDefs, dslParams)

    println(dslParams)
    ???
  }
}

private object NodeBuilder {
  def newIdxTermBuilder(cmd: TermCmd): IdxTermNodeBuilder = {
    new IdxTermNodeBuilder(cmd, None)
  }

  def newDslTermBuilder = ???
}

/** Not thread-safe */
private final class IdxTermNodeBuilder(cmd: TermCmd, lastSibling: Option[IdxTermNodeBuilder]) {

  private[this] var params: immutable.Seq[TermParam] = immutable.Seq.empty
  private[this] var opts: immutable.Seq[TermOpt] = immutable.Seq.empty

  def add(arg: TermArg): IdxTermNodeBuilder = arg match {
    case cmd: TermCmd => new IdxTermNodeBuilder(cmd, Option(this))
    case param: TermParam => this.params :+= param; this
    case opt: TermOpt => this.opts :+= opt; this
  }

  //non-defensive
  private def build: TermCmdNode = {
    //these CmdNodes are flat at level1 (level0 is the top)
    TermCmdNode(cmd, params, opts, parent = None, subCmdEntry = TermCommandEntry.default)
  }

  @inline
  def seal: immutable.Seq[TermCmdNode] = lastSibling match {
    case None => immutable.Seq(this.build)
    case Some(last) => last.seal :+ this.build
  }
}

private final class DslTermNodeBuilder(argDefs: immutable.Seq[TermArg],
                                       dslParams: immutable.Seq[Term.Arg]) {


  def recResolve(cmdTerm: Option[Term], params: immutable.Seq[Term.Arg]): TermCmdNode = {
    val singleArgs: immutable.Seq[TermArg] = params.collect {
      case Term.Name(argName) => queryArg(argName) match {
        case Some(termArg) => termArg
        case None => notDefined(argName)
      }
    }
    val groupArgs: immutable.Seq[(Limitation, Seq[TermArg])] = params.collect {
      case Term.ApplyInfix(subs, Term.Name(operator), _, Seq(Term.Name(lastArgName))) =>
        val relationSeq = recInfix2seq(subs)
          .map(argName => queryArg(argName).getOrElse(notDefined(argName)))
        val limitation = Limitation.fromOperator(operator)
        limitation -> relationSeq
    }

    val subCmdEntry = {
      val cmdEntryTerm = q"scmdRuntime.buildCmdEntry(true)"
      val childrTermCmdNode = params.collect {
        case q"$cmd(..$subParams)" => recResolve(Some(cmd), subParams)
      }
      TermCommandEntry(cmdEntryTerm, childrTermCmdNode)
    }

    ???
  }

  @tailrec
  private def recInfix2seq(subTerm: Term,
                           acc: immutable.Seq[String] = immutable.Seq()): immutable.Seq[String] =
    subTerm match {
      case Term.Name(argName) => argName +: acc
      case Term.ApplyInfix(subs, Term.Name(operator), _, Seq(Term.Name(argName)))
        if operator == "&" || operator == "|" => recInfix2seq(subs, argName +: acc)
      case bad => throw new IllegalArgumentException(s"Tree DSL cannot be parsed: $bad")
    }

  private def queryArg(name: String): Option[TermArg] = argDefs.find(_.name == name)

  private def notDefined(name: String): Nothing =
    throw new IllegalArgumentException(s"Arg in Tree DSL not defined yet:$name")
}