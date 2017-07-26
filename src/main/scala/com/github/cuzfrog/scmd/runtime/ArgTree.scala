package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.{Argument, CanFormPrettyString, Command, CommandEntry, OptionArg, Parameter}

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
  def entity: {val name: String}
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

private[runtime] sealed trait ValueNode extends Node {
  def value: Seq[String]
  def tpe: ClassTag[_] //specific data type, not includes Seq or List
}

private case class ParamNode[+T](entity: Parameter[T], value: Seq[String],
                                 isVariable: Boolean, tpe: ClassTag[_])
  extends ValueNode with NodeTag[ParamNode[T]]

private case class OptNode[+T: ClassTag](entity: OptionArg[T], value: Seq[String])
  extends ValueNode with NodeTag[OptNode[T]] {
  val tpe = implicitly[ClassTag[_]]
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
  implicit def canFormPrettyString[N <: Node]: CanFormPrettyString[N] = {
    case n: CmdNode => s"cmd:${n.entity.name}"
    case n: ParamNode[_] =>
      val ifVariable = if (n.isVariable) "..." else ""
      s"param$ifVariable: ${n.entity.name}[${n.tpe}] = ${n.value}"
    case n: OptNode[_] => s"opt: ${n.entity.name}[${n.tpe}] = ${n.value}"
    case _: CmdEntryNode => s"cmdEntry"
  }
}