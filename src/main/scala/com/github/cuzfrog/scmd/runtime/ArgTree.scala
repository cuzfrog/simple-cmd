package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.{ArgValue, Argument, CanFormPrettyString, Command, CommandEntry, MutualLimitation, OptionArg, Parameter, PropertyArg, ValueArgument, VariableValue}

import scala.reflect.ClassTag

private final case class ArgTree(appName: String,
                                 topParams: Seq[ParamNode[_]],
                                 topOpts: Seq[OptNode[_]],
                                 props: Seq[PropNode[_]],
                                 cmdEntry: CmdEntryNode,
                                 topLimitations: Seq[(MutualLimitation, Seq[scala.Symbol])] = Nil,
                                 globalLimitations: Seq[(MutualLimitation, Seq[scala.Symbol])] = Nil) {
  def toTopNode: CmdNode = CmdNode(
    entity = Command.topCmd.copy(name = appName),
    params = topParams,
    opts = topOpts,
    subCmdEntry = cmdEntry,
    limitations = topLimitations
  )
}

private sealed trait Node {
  def entity: Argument[_]
}

private case class CmdNode(entity: Command,
                           params: Seq[ParamNode[_]],
                           opts: Seq[OptNode[_]],
                           subCmdEntry: CmdEntryNode,
                           limitations: Seq[(MutualLimitation, Seq[scala.Symbol])] = Nil) extends Node

private case class CmdEntryNode(entity: CommandEntry,
                                children: Seq[CmdNode]) {
  lazy val mandatoryDownstreamCnt: Int = this.countMandatoryDownstream
}

private[runtime] sealed trait NodeTag[+N <: NodeTag[N]]

private[runtime] sealed trait ValueNode[T] extends Node {
  def entity: ValueArgument[T] with ArgValue[T]
  def value: Seq[String]
  def tpe: ClassTag[_] //specific data type, not includes Seq or List
  //tpe needs to be put in constructor parameter, .copy removes type info.
  def isVariable: Boolean = entity.isVariable
  def isMandatory: Boolean = entity.isMandatory
  def parent: scala.Symbol
  override def hashCode(): Int = entity.hashCode * 3 + 17
}

private case class ParamNode[T](entity: Parameter[T] with ArgValue[T],
                                value: Seq[String], tpe: ClassTag[_],
                                parent: scala.Symbol)
  extends ValueNode[T] with NodeTag[ParamNode[T]] {
  //equality depends on its entity's. Value is stripped off for parsing quick comparing.
  override def equals(obj: scala.Any): Boolean = {
    if (!this.canEqual(obj)) return false
    obj.asInstanceOf[ParamNode[_]].entity == this.entity
  }
  override def canEqual(that: Any): Boolean = that match {
    case _: ParamNode[_] => true
    case _ => false
  }
}

private case class OptNode[T](entity: OptionArg[T] with ArgValue[T],
                              value: Seq[String], tpe: ClassTag[_],
                              parent: scala.Symbol)
  extends ValueNode[T] with NodeTag[OptNode[T]] {

  //equality depends on its entity's. Value is stripped off for parsing quick comparing.
  override def equals(obj: scala.Any): Boolean = {
    if (!this.canEqual(obj)) return false
    obj.asInstanceOf[OptNode[_]].entity == this.entity
  }
  override def canEqual(that: Any): Boolean = that match {
    case _: OptNode[_] => true
    case _ => false
  }
  //todo: check if equals' overriding is correct.
  //todo: use macro to generate equality overriding.
}

private case class PropNode[T](entity: PropertyArg[T] with VariableValue[(String, T)],
                               value: Seq[String], tpe: ClassTag[_])
  extends Node {
  //equality depends on its entity's. Value is stripped off for parsing quick comparing.
  override def equals(obj: scala.Any): Boolean = {
    if (!this.canEqual(obj)) return false
    obj.asInstanceOf[PropNode[_]].entity == this.entity
  }
  override def canEqual(that: Any): Boolean = that match {
    case _: PropNode[_] => true
    case _ => false
  }
}

private object ArgTree {
  implicit val canFormPrettyString: CanFormPrettyString[ArgTree] = (a: ArgTree) => {
    val cmdNode = a.toTopNode

    def variableOrMandatory(n: ValueNode[_]): (String, String) = {
      val ifVariable = if (n.isVariable) "..." else ""
      val ifMandatory = if (n.isMandatory) "(Mandatory)" else "(Optional)"
      (ifVariable, ifMandatory)
    }

    def recMkPrettyString(cmdNode: CmdNode, indent: String = ""): String = {
      val cmd = indent + cmdNode.entity.name
      val params = cmdNode.params.map { n =>
        val (ifVariable, ifMandatory) = variableOrMandatory(n)
        s"$indent+-param$ifVariable: ${n.entity.name}[${n.tpe}] $ifMandatory"
      }
      val opts = cmdNode.opts.map { n =>
        val (ifVariable, ifMandatory) = variableOrMandatory(n)
        s"$indent+-opt$ifVariable: ${n.entity.name}[${n.tpe}] $ifMandatory"
      }
      val subCmds =
        cmdNode.subCmdEntry.children.map(n => recMkPrettyString(n, indent + "   "))
      val cmdEntry = if (subCmds.isEmpty) Seq.empty else Seq(s"$indent +-CmdEntry")
      val result: Seq[String] =
        Seq(cmd) ++ params ++ opts ++ cmdEntry ++ subCmds
      result.mkString(NEWLINE)
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