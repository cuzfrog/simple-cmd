package com.github.cuzfrog.scmd.macros

import scala.annotation.tailrec
import scala.collection.immutable
import scala.meta._

private object GraphBuilder {
  @inline
  def buildArgGraphByIdx(argDefs: immutable.Seq[TermArg]): TermArgGraph = {
    @tailrec
    def recAdd(builder: TermNodeBuilder, args: immutable.Seq[TermArg]): TermNodeBuilder = {
      if (args.isEmpty) builder
      else recAdd(builder.add(args.head), args.tail)
    }

    argDefs.collectFirst { case cmd: TermCmd => cmd } match {
      case None =>
        val params = argDefs.collect { case param: TermParam => param }
        val opts = argDefs.collect { case opt: TermOpt => opt }
        TermArgGraph(Nil, params, opts)
      case Some(cmd1) =>
        val topLevelOpts = argDefs.filter(_.idx < cmd1.idx).collect {
          case opt: TermOpt => opt
          case param: TermParam => abort("Parameters cannot be defined before first command.")
        }
        val tail = argDefs.filter(_.idx > cmd1.idx)
        val builder = NodeBuilder.newTermBuilder(cmd1)

        val commands = recAdd(builder, tail).seal
        TermArgGraph(commands, Nil, topLevelOpts)
    }
  }
}

private object NodeBuilder {
  def newTermBuilder(cmd: TermCmd): TermNodeBuilder = {
    new TermNodeBuilder(cmd, None)
  }
}

private final class TermNodeBuilder(cmd: TermCmd, lastSibling: Option[TermNodeBuilder]) {

  private[this] var params: immutable.Seq[TermParam] = immutable.Seq.empty
  private[this] var opts: immutable.Seq[TermOpt] = immutable.Seq.empty
  private[this] var children: immutable.Seq[TermCmdNode] = immutable.Seq.empty

  def add(arg: TermArg): TermNodeBuilder = arg match {
    case cmd: TermCmd => new TermNodeBuilder(cmd, Option(this))
    case param: TermParam => this.params :+= param; this
    case opt: TermOpt => this.opts :+= opt; this
  }

  def addChild(node: TermCmdNode): this.type = {
    this.children :+= node
    this
  }

  //non-defensive
  private def build: TermCmdNode = TermCmdNode(cmd, params, opts, children)

  @tailrec
  private def lastSeal: immutable.Seq[TermCmdNode] = lastSibling match {
    case None => immutable.Seq.empty
    case Some(last) => last.lastSeal
  }

  @inline
  def seal: immutable.Seq[TermCmdNode] = lastSeal :+ this.build
}