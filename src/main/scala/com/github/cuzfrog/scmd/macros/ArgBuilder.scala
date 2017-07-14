package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.{Argument, Command, OptionArg, Parameter}

import scala.annotation.tailrec
import scala.meta._


private object ArgBuilder {
  @inline def buildArgGraphByIdx(argDefs: Seq[RawArg]): RawArgGraph = {
    val topLevelOpts = argDefs.takeWhile(_.arg.isInstanceOf[OptionArg[_]])
    val tail = argDefs.drop(topLevelOpts.length)
    if (tail.isEmpty) abort("Not enough args defined.")
    val (cmd1 :: rest) = tail
    if (!cmd1.arg.isInstanceOf[Command]) abort("Only options can be defined before command.")
    val builder = NodeBuilder.newBuilder(cmd1.arg.asInstanceOf[Command])

    @tailrec
    @inline
    def recAdd(builder: NodeBuilder, args: Seq[RawArg]): NodeBuilder = {
      if (args.isEmpty) builder
      else {
        val head = args.head
        recAdd(builder.add(head.arg, head.tpe), args.tail)
      }
    }

    val commands = recAdd(builder, rest).seal
    RawArgGraph(commands, topLevelOpts.map(r => (r.arg.asInstanceOf[OptionArg[_]], r.tpe)))
  }

  private def reifyRawGraph(rawArgGraph: RawArgGraph):Defn.Val={

    def recReifyCommand(rawCmdNode: RawCmdNode)={
      rawCmdNode.cmd
    }

    val cmd = rawArgGraph.commands.head.cmd


    q"val argGraph = 1"
  }
}


private object NodeBuilder {
  def newBuilder(cmd: Command): NodeBuilder = {
    new NodeBuilder(cmd, None)
  }
}

private final class NodeBuilder(cmd: Command, lastSibling: Option[NodeBuilder]) {

  private var params: Seq[(Parameter[_], Type)] = Seq.empty
  private var opts: Seq[(OptionArg[_], Type)] = Seq.empty
  private var children: Seq[RawCmdNode] = Seq.empty

  def add(arg: Argument[_], tpe: Type): NodeBuilder = arg match {
    case cmd: Command => new NodeBuilder(cmd, Option(this))
    case param: Parameter[_] => this.params :+= (param, tpe); this
    case opt: OptionArg[_] => this.opts :+= (opt, tpe); this
  }

  def addChild(node: RawCmdNode): this.type = {
    this.children :+= node
    this
  }

  private def build: RawCmdNode = RawCmdNode(cmd, params, opts, children)

  @tailrec
  private def lastSeal: Seq[RawCmdNode] = lastSibling match {
    case None => Seq(this.build)
    case Some(last) => last.lastSeal
  }

  def seal: Seq[RawCmdNode] = lastSeal :+ this.build
}