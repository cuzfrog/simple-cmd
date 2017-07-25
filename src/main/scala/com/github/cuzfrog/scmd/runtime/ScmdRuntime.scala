package com.github.cuzfrog.scmd.runtime

import java.util.concurrent.atomic.AtomicInteger

import com.github.cuzfrog.scmd.{AppInfo, Command, CommandEntry, Defaults, OptionArg, Parameter}

import scala.reflect.ClassTag
import scala.collection.mutable

/**
  * Util/helper class to build scmd runtime classes that is instantiated within client class.
  *
  * To privatize/capsulate other scmd classes for not to pollute client workspace.
  *
  * This is the only public class exposed to client.
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
                        default: Option[T] = None): Int

  def buildOptionArg[T](name: String,
                        abbr: Option[String] = None,
                        description: Option[String] = None,
                        isMandatory: Boolean = Defaults.isMandatory,
                        default: Option[T] = None): Int

  def buildParamNode[T](entity: Int, value: Seq[String],
                        isVariable: Boolean, tpe: ClassTag[_]): Int

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

  def addValidation(name: String, func: (_) => Unit): Unit
  /** Convert string value to typed value and validate it with previously provided function. */
  def validate[T: ClassTag](valueNode: ValueNode, value: Seq[String])
                           (implicit typeEvidence: ArgTypeEvidence[T]): T

  def argTreeString: String
  def appInfoString: String
  
}
object ScmdRuntime {
  def create: ScmdRuntime = new ScmdRuntimeImpl
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
  private[this] val nodeRefs = mutable.Map.empty[String, Node] //name -> id of node
  private[this] val valiRefs = mutable.Map.empty[ValueNode, (_) => Unit] //id -> func

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
                                 isMandatory: Boolean,
                                 default: Option[T]): Int = {
    val id = idGen.getAndIncrement()
    val a = Parameter[T](name = name,
      description = description,
      isMandatory = isMandatory,
      default = default)
    repository.put(id, Box(a))
    id
  }
  override def buildOptionArg[T](name: String,
                                 abbr: Option[String],
                                 description: Option[String],
                                 isMandatory: Boolean,
                                 default: Option[T]): Int = {
    val id = idGen.getAndIncrement()
    val a = OptionArg[T](name = name,
      abbr = abbr,
      description = description,
      isMandatory = isMandatory,
      default = default)
    repository.put(id, Box(a))
    id
  }
  override def buildParamNode[T](entity: Int, value: Seq[String],
                                 isVariable: Boolean, tpe: ClassTag[_]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[Parameter[T]](entity)
    val a = ParamNode[T](entity = e, value = value, isVariable = isVariable, tpe)
    repository.put(id, Box(a))
    nodeRefs.put(e.name, a)
    id
  }
  override def buildOptNode[T: ClassTag](entity: Int, value: Seq[String]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[OptionArg[T]](entity)
    val a = OptNode[T](entity = e, value = value)
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
  override def argTreeString: String = argTree.prettyString
  override def appInfoString: String = appInfo.prettyString
  override def addValidation(name: String, func: (_) => Unit): Unit = {
    nodeRefs.get(name) match {
      case Some(node: ValueNode) => valiRefs.put(node, func)
      case Some(node) => throw new AssertionError(s"Node[$node] with name$name is not a value node.")
      case None => throw new AssertionError(s"Cannot find node with name$name")
    }
  }
  override def validate[T: ClassTag](valueNode: ValueNode,
                                     value: Seq[String])
                                    (implicit typeEvidence: ArgTypeEvidence[T]): T = {
    val tpe = implicitly[ClassTag[T]]
    if (tpe != valueNode.tpe)
      throw new AssertionError(s"Type of demanded value is different from node's")
    val typedValue = typeEvidence.verify(value)
    valiRefs.get(valueNode).foreach { basicValidationFunc =>
      basicValidationFunc.asInstanceOf[T => Unit].apply(typedValue)
    }
    typedValue
  }

}