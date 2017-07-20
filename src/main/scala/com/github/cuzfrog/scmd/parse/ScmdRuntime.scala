package com.github.cuzfrog.scmd.parse

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
  def addAppInfo(name: String,
                 shortDescription: Option[String] = None,
                 fullDescription: Option[String] = None,
                 version: Option[String] = None,
                 license: Option[String] = None,
                 author: Option[String] = None,
                 custom: Map[String, String] = Map.empty): this.type

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

  def buildParamNode[T](entity: Int,
                        tpe: ClassTag[T],
                        value: Seq[String]): Int

  def buildOptNode[T](entity: Int,
                      tpe: ClassTag[T],
                      value: Seq[String]): Int

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

  def argTreeString: String
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

  private[this] var appInfo: AppInfo = _
  private[this] var argTree: ArgTree = _
  private[this] val repository = mutable.Map.empty[Int, Box[_]]
  private[this] val idGen = new AtomicInteger(0)

  private def getEntity[T: ClassTag](e: Int): T =
    repository.getOrElse(e, throw new AssertionError("Recusive build failed.")).unbox[T]

  override def addAppInfo(name: String,
                          shortDescription: Option[String],
                          fullDescription: Option[String],
                          version: Option[String],
                          license: Option[String],
                          author: Option[String],
                          custom: Map[String, String]): this.type = {
    appInfo = AppInfo(name = name,
      shortDescription = shortDescription,
      fullDescription = fullDescription,
      version = version,
      license = license,
      author = author,
      custom = custom)
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
  override def buildParamNode[T](entity: Int,
                                 tpe: ClassTag[T],
                                 value: Seq[String]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[Parameter[T]](entity)//.asInstanceOf[Parameter[T]]
    val a = ParamNode[T](entity = e, tpe = tpe, value = value)
    repository.put(id, Box(a))
    id
  }
  override def buildOptNode[T](entity: Int,
                               tpe: ClassTag[T],
                               value: Seq[String]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[OptionArg[T]](entity)//.asInstanceOf[OptionArg[T]]
    val a = OptNode[T](entity = e, tpe = tpe, value = value)
    repository.put(id, Box(a))
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
}