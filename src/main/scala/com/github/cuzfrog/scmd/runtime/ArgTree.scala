package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.{ArgValue, Argument, CanFormPrettyString, Command, CommandEntry, OptionArg, Parameter, SingleValue}

import scala.reflect.ClassTag

private final case class ArgTree(topParams: Seq[ParamNode[_]],
                                 topOpts: Seq[OptNode[_]],
                                 cmdEntry: CmdEntryNode) {
  def toTopNode: CmdNode = CmdNode(
    parent = None,
    params = topParams,
    opts = topOpts,
    subCmdEntry = cmdEntry,
    entity = Command("AppName", None) //todo: replace AppName
  )
}

private sealed trait Node {
  def entity: Argument[_]
}

private case class CmdNode(entity: Command,
                           params: Seq[ParamNode[_]],
                           opts: Seq[OptNode[_]],
                           parent: Option[CmdNode],
                           subCmdEntry: CmdEntryNode) extends Node

private case class CmdEntryNode(entity: CommandEntry,
                                children: Seq[CmdNode]) extends Node {
  lazy val mandatoryDownstreamCnt: Int = this.countMandatoryDownstream
}

private[runtime] sealed trait NodeTag[+N <: NodeTag[N]]

private[runtime] sealed trait ValueNode[T] extends Node {
  def value: Seq[String]
  def tpe: ClassTag[_] //specific data type, not includes Seq or List
  //tpe needs to be put in constructor parameter, .copy removes type info.
}

private case class ParamNode[T](entity: Parameter[T] with ArgValue[T],
                                value: Seq[String], tpe: ClassTag[_])
  extends ValueNode[T] with NodeTag[ParamNode[T]] {
  def isVariable: Boolean = entity.isVariable
}

private case class OptNode[T](entity: OptionArg[T] with ArgValue[T],
                              value: Seq[String], tpe: ClassTag[_])
  extends ValueNode[T] with NodeTag[OptNode[T]] {
  def isVariable: Boolean = entity.isVariable
  //OptNode's equality depends on its entity's. Value is stripped off for parsing quick comparing.
  override def hashCode(): Int = entity.hashCode * 3 + 17
  override def equals(obj: scala.Any): Boolean = {
    if (!this.canEqual(obj)) return false
    obj.asInstanceOf[OptNode[_]].entity == this.entity
  }
  override def canEqual(that: Any): Boolean = that match {
    case _: OptNode[_] => true
    case _ => false
  }
  //todo: check if equals' overriding is correct.
}

private object ArgTree {
  implicit val canFormPrettyString: CanFormPrettyString[ArgTree] = (a: ArgTree) => {
    val NEW_LINE = System.lineSeparator
    val cmdNode = a.toTopNode

    def recMkPrettyString(cmdNode: CmdNode, indent: String = ""): String = {
      val cmd = indent + cmdNode.entity.name
      val params =
        cmdNode.params.map { n =>
          val ifVariable = if (n.isVariable) "..." else ""
          s"$indent+-param$ifVariable: ${n.entity.name}[${n.tpe}] = ${n.value}"
        }
      val opts =
        cmdNode.opts.map(n => s"$indent+-opt: ${n.entity.name}[${n.tpe}] = ${n.value}")
      val subCmds =
        cmdNode.subCmdEntry.children.map(n => recMkPrettyString(n, indent + "   "))
      val cmdEntry = if (subCmds.isEmpty) Seq.empty else Seq(s"$indent +-CmdEntry")
      val result: Seq[String] =
        Seq(cmd) ++ params ++ opts ++ cmdEntry ++ subCmds
      result.mkString(NEW_LINE)
    }

    recMkPrettyString(cmdNode)
  }
}

private object Node {
  implicit def canFormPrettyString[N <: Node]: CanFormPrettyString[N] = recPrettyString

  private def recPrettyString[N <: Node](a: N): String = a match {
    case n: CmdNode =>
      s"cmd:${n.entity.name} +params(${n.params.size}) +opts(${n.opts.size})"
    case n: ParamNode[_] =>
      val ifVariable = if (n.isVariable) "..." else ""
      s"param$ifVariable: ${n.entity.name}[${n.tpe}] = ${n.value}"
    case n: OptNode[_] =>
      val ifMultiple = if (n.isVariable) "..." else ""
      s"opt$ifMultiple: ${n.entity.name}[${n.tpe}] = ${n.value}"
    case _: CmdEntryNode => s"cmdEntry"
  }
}