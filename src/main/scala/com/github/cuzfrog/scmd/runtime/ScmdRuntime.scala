package com.github.cuzfrog.scmd.runtime

import java.util.concurrent.atomic.AtomicInteger


import com.github.cuzfrog.scmd.runtime.logging.ScmdRuntimeLogging
import com.github.cuzfrog.scmd._

import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.reflect.ClassTag

/**
  * Util/helper class to build scmd runtime classes that is instantiated within client class.
  * <br><br>
  * This is a public class exposed to client,
  * to privatize/encapsulate other scmd classes for not to pollute client workspace.
  */
sealed trait ScmdRuntime {
  def addAppInfo(name: Option[String] = None,
                 shortDescription: Option[String] = None,
                 fullDescription: Option[String] = None,
                 version: Option[String] = None,
                 license: Option[String] = None,
                 author: Option[String] = None,
                 custom: Seq[(String, String)] = Seq.empty): this.type

  def buildCommand(name: String,
                   description: Option[String]): Int

  def buildParameter[T](name: String,
                        description: Option[String] = None,
                        isMandatory: Boolean = Defaults.isMandatory,
                        argValue: ArgValue[T]): Int

  def buildOptionArg[T](name: String,
                        abbr: Option[String] = None,
                        description: Option[String] = None,
                        isMandatory: Boolean = Defaults.isMandatory,
                        argValue: ArgValue[T]): Int

  def buildSingleValue[T](_default: Option[T]): ArgValue[T]
  def buildVariableValue[T](_default: Seq[T]): ArgValue[T]

  def buildParamNode[T: ClassTag](entity: Int, value: Seq[String]): Int

  def buildOptNode[T: ClassTag](entity: Int, value: Seq[String]): Int

  def buildCmdEntryNode(entity: Int,
                        children: Seq[Int]): Int

  def defaultCommandEntry: Int

  def buildCmdNode(entity: Int,
                   params: Seq[Int],
                   opts: Seq[Int],
                   parent: Option[Int],
                   subCmdEntry: Int): Int

  def buildArgTree(topParams: Seq[Int],
                   topOpts: Seq[Int],
                   cmdEntry: Int): this.type

  def addValidation[T](name: String, func: T => Unit): Unit

  /** Trigger final parsing. Return parsed nodes. */
  def parse(args: Seq[String]): Seq[String]

  /** Return not parsed node. */
  def getNodeByName[N <: Node : ClassTag](name: String): N

  /**
    * Key method to return parsed/evaluated argument to client def class.
    *
    * Validation is done before returning.
    * String values of cmd args have been converted into typed values within validation.
    *
    * @see [[com.github.cuzfrog.scmd.runtime.ArgTypeEvidence]]<br>
    *      [[com.github.cuzfrog.scmd.macros.argutils.ConvertParsedImpl]]<br>
    *      [[com.github.cuzfrog.scmd.runtime.Validator]]
    * @param name the name of the argument, String.
    * @tparam T the value type of the argument, retained by macro.
    * @tparam A the Argument type, delivered by macro.
    * @return an evaluated Argument.
    */
  def getEvaluatedArgumentByName
  [T: ClassTag : ArgTypeEvidence, A <: Argument[T] : ClassTag](name: String): A

  def argTreeString: String
  def appInfoString: String
  def parsedSeqString: String
}
object ScmdRuntime {
  def create: ScmdRuntime = new ScmdRuntimeImpl with ScmdRuntimeLogging
}

/** Not Thread-safe. It is only executed privately inside @ScmdDef annotated class, at runtime. */
private class ScmdRuntimeImpl extends ScmdRuntime {

  private case class Box[A: ClassTag](private val a: A) {
    private val tpe = implicitly[ClassTag[A]]
    /** */
    def unbox[T: ClassTag]: T = {
      val tt = implicitly[ClassTag[T]]
      if (tt == tpe) a.asInstanceOf[T]
      else throw new AssertionError("Unbox with wrong type.")
    }
  }
  private[this] val idGen = new AtomicInteger(0)

  private[this] var appInfo: AppInfo = _
  private[this] var argTree: ArgTree = _
  private[this] val repository = mutable.Map.empty[Int, Box[_]] //id -> element
  private[this] val nodeRefs = mutable.Map.empty[String, Node] //name -> node
  private[this] val valiRefs = mutable.Map.empty[ValueNode[_], Function1[_, Unit]] //id -> func
  private[this] val parsedNodes = mutable.LinkedHashMap.empty[String, Node] //name -> node
  private[this] val parsedContextSnapshots = mutable.Map.empty[Node, ContextSnapshot]

  private def getEntity[T: ClassTag](e: Int): T =
    repository.getOrElse(e, throw new AssertionError("Recursive build failed.")).unbox[T]

  override def addAppInfo(name: Option[String],
                          shortDescription: Option[String],
                          fullDescription: Option[String],
                          version: Option[String],
                          license: Option[String],
                          author: Option[String],
                          custom: Seq[(String, String)]): this.type = {
    appInfo = AppInfo(name = name,
      shortDescription = shortDescription,
      fullDescription = fullDescription,
      version = version,
      license = license,
      author = author,
      custom = custom.to[scala.collection.immutable.Seq])
    this
  }
  override def buildCommand(name: String, description: Option[String]): Int = {
    val id = idGen.getAndIncrement()
    val a = Command(name = name, description = description)
    repository.put(id, Box(a))
    id
  }
  override def buildParameter[T](name: String,
                                 description: Option[String],
                                 isMandatory: Boolean, argValue: ArgValue[T]): Int = {
    val id = idGen.getAndIncrement()
    val a = mix(Parameter[T](name, description, isMandatory), argValue)
    repository.put(id, Box(a))
    id
  }
  override def buildOptionArg[T](name: String,
                                 abbr: Option[String],
                                 description: Option[String],
                                 isMandatory: Boolean, argValue: ArgValue[T]): Int = {
    val id = idGen.getAndIncrement()
    val a = mix(OptionArg[T](name, abbr, description, isMandatory), argValue)
    repository.put(id, Box(a))
    id
  }
  override def buildSingleValue[T](_default: Option[T]): ArgValue[T] = {
    ArgValue.single(_default)
  }
  override def buildVariableValue[T](_default: Seq[T]): ArgValue[T] = {
    ArgValue.variable(_default)
  }
  override def buildParamNode[T: ClassTag](entity: Int, value: Seq[String]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[Parameter[T] with ArgValue[T]](entity)
    val a = ParamNode[T](entity = e, value = value, tpe = implicitly[ClassTag[T]])
    repository.put(id, Box(a))
    nodeRefs.put(e.name, a)
    id
  }
  override def buildOptNode[T: ClassTag](entity: Int, value: Seq[String]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[OptionArg[T] with ArgValue[T]](entity)
    val a = OptNode[T](entity = e, value = value, tpe = implicitly[ClassTag[T]])
    repository.put(id, Box(a))
    nodeRefs.put(e.name, a)
    id
  }
  override def buildCmdEntryNode(entity: Int, children: Seq[Int]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[CommandEntry](entity)
    val c = children.map(getEntity[CmdNode])
    val a = CmdEntryNode(e, c)
    repository.put(id, Box(a))
    id
  }
  override def defaultCommandEntry: Int = {
    val id = idGen.getAndIncrement()
    val a = CommandEntry("")
    repository.put(id, Box(a))
    id
  }
  override def buildCmdNode(entity: Int,
                            params: Seq[Int],
                            opts: Seq[Int],
                            parent: Option[Int],
                            subCmdEntry: Int): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[Command](entity)
    val p = params.map(getEntity[ParamNode[_]])
    val o = opts.map(getEntity[OptNode[_]])
    val pa = parent.map(getEntity[CmdNode])
    val se = getEntity[CmdEntryNode](subCmdEntry)
    val a = CmdNode(e, p, o, pa, se)
    repository.put(id, Box(a))
    nodeRefs.put(e.name, a)
    id
  }
  override def buildArgTree(topParams: Seq[Int],
                            topOpts: Seq[Int],
                            cmdEntry: Int): ScmdRuntimeImpl.this.type = {
    val tp = topParams.map(getEntity[ParamNode[_]])
    val to = topOpts.map(getEntity[OptNode[_]])
    val ce = getEntity[CmdEntryNode](cmdEntry)
    argTree = ArgTree(tp, to, ce)
    repository.clear()
    this
  }
  override def addValidation[T](name: String, func: T => Unit): Unit = {
    nodeRefs.get(name) match {
      case Some(node: ValueNode[T@unchecked]) => valiRefs.put(node, func)
      case Some(node) =>
        throw new AssertionError(s"Node[$node] with name$name is not a value node" +
          s"(so not compatible with validation).")
      case None => throw new AssertionError(s"Cannot find node with name$name")
    }
  }
  override def parse(args: Seq[String]): Seq[String] = {
    if (parsedNodes.nonEmpty) throw new IllegalStateException("ScmdRuntime cannot parse args twice.")
    ArgParser.parse(argTree, args).map {
      case (node, cs) =>
        parsedNodes.put(node.entity.name, node)
        parsedContextSnapshots.put(node, cs)
    }
    parsedNodes.keys.toSeq
  }
  override def getNodeByName[N <: Node : ClassTag](name: String): N = {
    this.getNode[N](name, nodeRefs) match {
      case Some(n) => n
      case None =>
        throw new NoSuchElementException(s"Node of name:$name cannot be found in runtime.")
    }
  }

  /** (private) Given name and type, query a node from a provided map. */
  private def getNode[N <: Node : ClassTag](name: String,
                                            refs: mutable.Map[String, Node]): Option[N] = {
    val tpe = implicitly[ClassTag[N]].runtimeClass
    val node: Option[Node] = refs.get(name).map {
      case cmdNode: CmdNode if tpe == classOf[CmdNode] => cmdNode
      case paramNode: ParamNode[_]
        if tpe == classOf[ParamNode[_]] || tpe == classOf[ValueNode[_]] => paramNode
      case optNode: OptNode[_]
        if tpe == classOf[OptNode[_]] || tpe == classOf[ValueNode[_]] => optNode
      case _: CmdEntryNode =>
        throw new AssertionError(s"CmdEntryNode has no name and should not be cached.")
      case bad =>
        throw new MatchError(s"Node of specified type[$tpe] has not specified name:$name.$bad")
    }
    node.map(_.asInstanceOf[N])
  }

  override def getEvaluatedArgumentByName
  [T: ClassTag : ArgTypeEvidence, A <: Argument[T] : ClassTag](name: String): A = {
    if (parsedNodes.isEmpty) throw new AssertionError("Parsed node empty before query by name.")
    val valueTpe = implicitly[ClassTag[T]]
    val argTpe = implicitly[ClassTag[A]]

    /** Get parsed node by type. */
    def parsedNode[N <: Node : ClassTag]: Option[N] = this.getNode[N](name, parsedNodes)
    def checkNodeType(n: ValueNode[_]): Unit = if (n.tpe != valueTpe)
      throw new AssertionError(s"Node's value type[${n.tpe}]" +
        s" is different from specified type[$valueTpe].")

    val argument: Argument[_] = argTpe.runtimeClass match {
      case rc if rc == classOf[Command] =>
        val node = this.getNodeByName[CmdNode](name)
        node.entity.copy(met = parsedNodes.get(name).nonEmpty)

      case rc if rc == classOf[Parameter[_]] || rc == classOf[OptionArg[_]] =>
        val node = this.getNodeByName[ValueNode[_]](name)
        checkNodeType(node)
        val value: Seq[T] = parsedNode[ValueNode[T]] match {
          case None => Seq.empty[T]
          case Some(n) =>
            val cs = parsedContextSnapshots
              .getOrElse(n, throw new AssertionError("ContextSnapshot should company parsed node. "))
            Validator.validate(n, cs, valiRefs.get(n))
        }
        merge(node.entity.asInstanceOf[ValueArgument[T]], value)
    }
    argument.asInstanceOf[A]
  }


  override def argTreeString: String = argTree.prettyString
  override def appInfoString: String = appInfo.prettyString
  override def parsedSeqString: String = parsedNodes.values.toSeq.prettyString
}