package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.ScmdUtils.CanFormPrettyString
import com.github.cuzfrog.scmd.internal.EqualityOverridingMacro
import com.github.cuzfrog.scmd._

import scala.reflect.ClassTag

private final case class ArgTree(appInfo: AppInfo,
                                 topParams: Seq[ParamNode[_]],
                                 topOpts: Seq[OptNode[_]],
                                 priors: Seq[PriorNode],
                                 props: Seq[PropNode[_]],
                                 cmdEntry: CmdEntryNode,
                                 topLimitations: Seq[LimitationTree] = Nil,
                                 globalLimitations: Seq[LimitationTree] = Nil) {
  def toTopNode: CmdNode = CmdNode(
    entity = Command.topCmd(appInfo.name).copy(description = appInfo.shortDescription),
    params = topParams,
    opts = topOpts,
    subCmdEntry = cmdEntry,
    limitations = topLimitations
  )
}

private sealed trait Node extends Product with Serializable {
  def entity: Argument[_]
}

private case class CmdNode(entity: Command,
                           params: Seq[ParamNode[_]],
                           opts: Seq[OptNode[_]],
                           subCmdEntry: CmdEntryNode,
                           limitations: Seq[LimitationTree] = Nil) extends Node

private case class CmdEntryNode(entity: CommandEntry,
                                children: Seq[CmdNode]) {
  lazy val mandatoryDownstreamCnt: Int = this.mandatoriesDownstream.size
  lazy val mandatoriesDownstream: Seq[Node] = this.getMandatoriesDownstream
}

private case class PriorNode(entity: PriorArg, parent: scala.Symbol) extends Node

private[runtime] sealed trait NodeTag[+N <: NodeTag[N]]

private[runtime] sealed trait ValueNode[T] extends Node {
  def entity: ValueArgument[T] with ArgValue[T]
  def value: Seq[String]
  def tpe: ClassTag[_] //specific data type, not includes Seq or List
  //tpe needs to be put in constructor parameter, .copy removes type info.
  def isVariable: Boolean = entity.isVariable
  def isMandatory: Boolean = entity.isMandatory
  def parent: scala.Symbol
}

/*
 * For ParamNode, OptNode, PropNode, equality depends on its entity's.
 * Value is stripped off for parsing quick comparing.
 *
 * When parsing args, node and path are the basic idea.
 * see TryPath and Context.
 *
 * Check equality overriding: Done 2017-08-11. see: EqualityOverridingMacroTest
 */
@EqualityOverridingMacro
private case class ParamNode[T](entity: Parameter[T] with ArgValue[T],
                                value: Seq[String], tpe: ClassTag[_],
                                parent: scala.Symbol)
  extends ValueNode[T] with NodeTag[ParamNode[T]]
@EqualityOverridingMacro
private case class OptNode[T](entity: OptionArg[T] with ArgValue[T],
                              value: Seq[String], tpe: ClassTag[_],
                              parent: scala.Symbol)
  extends ValueNode[T] with NodeTag[OptNode[T]] {
  def addValue(v: String): OptNode[T] = this.copy(value = value :+ v)
}
@EqualityOverridingMacro
private case class PropNode[T](entity: PropertyArg[T] with VariableValue[(String, T)],
                               value: Seq[(String, String)], tpe: ClassTag[_]) extends Node

private object ArgTree {
  implicit val canFormPrettyString: CanFormPrettyString[ArgTree] =
    new CanFormPrettyString[ArgTree] {
      override def mkPrettyString(a: ArgTree): String = {
        val cmdNode = a.toTopNode
        recMkPrettyString(cmdNode, props = a.props, priors = a.priors)
      }
    }
  private def variableOrMandatory(n: ValueNode[_]): (String, String) = {
    val ifVariable = if (n.isVariable) "..." else ""
    val ifMandatory = if (n.isMandatory) "(Mandatory)" else "(Optional)"
    (ifVariable, ifMandatory)
  }
  private def recMkPrettyString(cmdNode: CmdNode, indent: String = "",
                                props: Seq[PropNode[_]] = Nil,
                                priors: Seq[PriorNode] = Nil): String = {
    val cmd = indent + cmdNode.entity.name
    val priorsStr = priors.map { n =>
      s"$indent+-prior: ${n.entity.name} alias: ${n.entity.alias.mkString(",")}"
    }
    val propsStr =
      props.map(p => s"+-props: ${p.entity.name}[key->${p.tpe}] flag ${p.entity.originalName}")
    val params = cmdNode.params.map { n =>
      val (ifVariable, ifMandatory) = variableOrMandatory(n)
      s"$indent+-param$ifVariable: ${n.entity.name}[${n.tpe}] $ifMandatory"
    }
    val opts = cmdNode.opts.map { n =>
      val (ifVariable, ifMandatory) = variableOrMandatory(n)
      s"$indent+-opt$ifVariable: ${n.entity.name}[${n.tpe}] $ifMandatory ${n.entity.originalName}"
    }
    val subCmds =
      cmdNode.subCmdEntry.children.map(n => recMkPrettyString(n, indent + "   "))
    val cmdEntry = if (subCmds.isEmpty) Seq.empty else Seq(s"$indent +-CmdEntry")
    val result: Seq[String] =
      Seq(cmd) ++ propsStr ++ priorsStr ++ params ++ opts ++ cmdEntry ++ subCmds
    result.mkString(NEWLINE)
  }
}

private object Node {
  implicit def canFormPrettyString[N <: Node]: CanFormPrettyString[N] =
    new CanFormPrettyString[N] {
      override def mkPrettyString(a: N): String = recPrettyString(a)
    }

  private def recPrettyString[N <: Node](a: N): String = a match {
    case n: CmdNode =>
      s"cmd:${n.entity.name} +params(${n.params.size}) +opts(${n.opts.size})"
    case n: PriorNode =>
      s"prior: ${n.entity.name} alias:${n.entity.alias.mkString(",")}"
    case n: ParamNode[_] =>
      val ifVariable = if (n.isVariable) "..." else ""
      s"param$ifVariable: ${n.entity.name}[${n.tpe}] = ${n.value}"
    case n: OptNode[_] =>
      val ifMultiple = if (n.isVariable) "..." else ""
      s"opt$ifMultiple: ${n.entity.name}[${n.tpe}] = ${n.value}"
    case n: PropNode[_] =>
      s"props: ${n.entity.name}(${n.entity.originalName})[key->${n.tpe}] = ${n.value.mkString("|")}"
    case _: CmdEntryNode => s"cmdEntry"
  }
}
