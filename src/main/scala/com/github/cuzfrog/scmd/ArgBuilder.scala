package com.github.cuzfrog.scmd


import scala.annotation.tailrec
import scala.meta._
import scala.reflect.ClassTag
import scala.collection.mutable

/**
  * Created by cuz on 7/13/17.
  */
private object ArgBuilder {
  @inline def buildArgGraphByIdx(argDefs: Seq[RawArg]): ArgGraph = {

    val topLevelOpts = argDefs.takeWhile(_.arg.isInstanceOf[OptionArg[_]])
    val tail = argDefs.drop(topLevelOpts.length)
    if (tail.isEmpty) abort("Not enough args defined.")
    val (cmd1 :: rest) = tail
    if (!cmd1.arg.isInstanceOf[Command]) abort("Only options can be defined before command.")
    val builder = CmdNode.newBuilder(cmd1.arg.asInstanceOf[Command])

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
    ArgGraph(commands, topLevelOpts.map(r => (r.arg.asInstanceOf[OptionArg[_]], r.tpe)))
  }
}

private final case class RawArg(arg: Argument[_], idx: Int, tpe: Type)

private final case class CmdNode(cmd: Command,
                                 params: Seq[(Parameter[_], Type)],
                                 opts: Seq[(OptionArg[_], Type)],
                                 children: Seq[CmdNode])

private object CmdNode {
  def newBuilder(cmd: Command): NodeBuilder = {
    new NodeBuilder(cmd, None)
  }
}

private final class NodeBuilder(cmd: Command, lastSibling: Option[NodeBuilder]) {


  private var params: Seq[(Parameter[_], Type)] = Seq.empty
  private var opts: Seq[(OptionArg[_], Type)] = Seq.empty
  private var children: Seq[CmdNode] = Seq.empty

  def add(arg: Argument[_], tpe: Type): NodeBuilder = arg match {
    case cmd: Command => new NodeBuilder(cmd, Option(this))
    case param: Parameter[_] => this.params :+= (param, tpe); this
    case opt: OptionArg[_] => this.opts :+= (opt, tpe); this
  }

  def addChild(node: CmdNode): this.type = {
    this.children :+= node
    this
  }

  private def build: CmdNode = CmdNode(cmd, params, opts, children)

  @tailrec
  private def lastSeal: Seq[CmdNode] = lastSibling match {
    case None => Seq(this.build)
    case Some(last) => last.lastSeal
  }

  def seal: Seq[CmdNode] = lastSeal :+ this.build

}

private final case class ArgGraph(commands: Seq[CmdNode],
                                  opts: Seq[(OptionArg[_], Type)])