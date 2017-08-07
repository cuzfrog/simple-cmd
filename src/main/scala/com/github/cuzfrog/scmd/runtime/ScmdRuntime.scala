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

  def buildPropertyArg[T](name: String,
                          flag: String,
                          description: Option[String] = None,
                          variableValue: VariableValue[(String, T)]): Int

  def buildPriorArg(name: String,
                    alias: Seq[String],
                    description: Option[String],
                    matchName: Boolean): Int

  def builtInArgs(name: scala.Symbol): Int

  def buildCmdEntry(isMandatory: Boolean = Defaults.isMandatory): Int

  def buildSingleValue[T](_default: Option[T]): SingleValue[T]
  def buildVariableValue[T](_default: Seq[T]): VariableValue[T]

  def buildParamNode[T: ClassTag](entity: Int, value: Seq[String], parent: scala.Symbol): Int

  def buildOptNode[T: ClassTag](entity: Int, value: Seq[String], parent: scala.Symbol): Int

  def buildPropNode[T: ClassTag](entity: Int, value: Seq[(String, String)]): Int

  def buildPriorNode(entity: Int, parent: scala.Symbol): Int

  def buildCmdEntryNode(entity: Int,
                        children: Seq[Int]): Int

  def buildCmdNode(entity: Int,
                   params: Seq[Int],
                   opts: Seq[Int],
                   priors: Seq[Int],
                   subCmdEntry: Int,
                   limitations: Seq[(MutualLimitation, Seq[scala.Symbol])] = Nil): Int

  def buildArgTree(appName: String,
                   topParams: Seq[Int],
                   topOpts: Seq[Int],
                   topPriors: Seq[Int],
                   props: Seq[Int],
                   cmdEntry: Int,
                   topLimitations: Seq[(MutualLimitation, Seq[scala.Symbol])] = Nil,
                   globalLimitations: Seq[(MutualLimitation, Seq[scala.Symbol])] = Nil): this.type

  def addValidation[T](name: String, func: T => Unit): Unit

  /**
    * Trigger final parsing.
    * <br><br>
    * First it uses ArgParser to parse args against ArgTree(defined already).
    * If parsing succeeds, it does mutual limitation validation.
    * Then it restores the result in runtime cache.
    * <br><br>
    * This is one of the two scmd-runtime Apis that throws scmd exceptions. The other is:<br>
    * {{{    def getEvaluatedArgumentByName
    *  [T: ClassTag : ArgTypeEvidence, A <: Argument[T] : ClassTag](name: scala.Symbol): A }}}
    *
    * @return parsed argument name sequence.
    */
  @throws[ArgParseException]("when parsing failed.")
  @throws[ArgValidationException]("when mutual limitation validation failed.")
  def parse(args: Seq[String]): Seq[String]


  /**
    * Key method to return parsed/evaluated argument to client def class.
    * <br><br>
    * Validation is done before returning.
    * String values of cmd args have been converted into typed values within validation.
    *
    * This is one of the two scmd-runtime Apis that throws scmd exceptions. The other is:<br>
    * {{{    def parse(args: Seq[String]) }}}
    *
    * @see [[com.github.cuzfrog.scmd.runtime.ArgTypeEvidence]]<br>
    *      [[com.github.cuzfrog.scmd.macros.argutils.ConvertParsedImpl]]<br>
    *      [[com.github.cuzfrog.scmd.runtime.Validator]]
    * @param name the name of the argument.
    * @tparam T the value type of the argument, retained by macro.
    * @tparam A the Argument type, delivered by macro.
    * @return an evaluated Argument.
    */
  @throws[ArgValidationException]("when type validation failed.")
  def getEvaluatedArgumentByName
  [T: ClassTag : ArgTypeEvidence, A <: Argument[T] : ClassTag](name: scala.Symbol): A

  def runBuiltInPriors(): Unit

  def handleException[E <: ScmdException : ScmdExceptionHandler](e: E): Nothing

  /** Clean cache to release references. */
  def clean(): Unit

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

  private[this] var appInfo: Option[AppInfo] = None
  private[this] var argTree: Option[ArgTree] = None
  private[this] val repository = mutable.Map.empty[Int, Box[_]] //id -> element
  private[this] val nodeRefs = mutable.Map.empty[scala.Symbol, Node] //name -> node
  private[this] val valiRefs = mutable.Map.empty[Node {def tpe: ClassTag[_]}, Function1[_, Unit]] //id -> func
  private[this] val parsedNodes = mutable.LinkedHashMap.empty[scala.Symbol, Node] //name -> node
  private[this] val parsedContextSnapshots = mutable.Map.empty[Node, ContextSnapshot]

  //----debug methods-----
  protected final def getNodeRefs: Map[scala.Symbol, Node] = nodeRefs.toMap
  //----debug methods-----

  private def getEntity[T: ClassTag](e: Int): T =
    repository.getOrElse(e, throw new AssertionError("Recursive build failed.")).unbox[T]

  override def addAppInfo(name: Option[String],
                          shortDescription: Option[String],
                          fullDescription: Option[String],
                          version: Option[String],
                          license: Option[String],
                          author: Option[String],
                          custom: Seq[(String, String)]): this.type = {
    appInfo = Some(AppInfo(name = name,
      shortDescription = shortDescription,
      fullDescription = fullDescription,
      version = version,
      license = license,
      author = author,
      custom = custom.to[scala.collection.immutable.Seq]))
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
  override def buildPropertyArg[T](name: String,
                                   flag: String,
                                   description: Option[String],
                                   variableValue: VariableValue[(String, T)]): Int = {
    val id = idGen.getAndIncrement()
    val a = mix(PropertyArg[T](name, flag, description), variableValue)
    repository.put(id, Box(a))
    id
  }
  override def buildPriorArg(name: String,
                             alias: Seq[String],
                             description: Option[String],
                             matchName: Boolean): Int = {
    val id = idGen.getAndIncrement()
    val a = PriorArg(name, alias = alias, description = description, matchName = matchName)
    repository.put(id, Box(a))
    id
  }
  override def builtInArgs(name: scala.Symbol): Int = {
    val id = idGen.getAndIncrement()
    val a = Argument.builtInArgs.getOrElse(name,
      throw new IllegalArgumentException(s"Cannot find built in arg:${name.name}"))
    repository.put(id, Box(a))
    id
  }
  override def buildCmdEntry(isMandatory: Boolean = Defaults.isMandatory): Int = {
    val id = idGen.getAndIncrement()
    val a = CommandEntry(isMandatory)
    repository.put(id, Box(a))
    id
  }
  override def buildSingleValue[T](_default: Option[T]): SingleValue[T] = {
    ArgValue.single(_default)
  }
  override def buildVariableValue[T](_default: Seq[T]): VariableValue[T] = {
    ArgValue.variable(_default)
  }
  override def buildParamNode[T: ClassTag](entity: Int,
                                           value: Seq[String],
                                           parent: scala.Symbol): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[Parameter[T] with ArgValue[T]](entity)
    val a = ParamNode[T](entity = e, value = value, tpe = implicitly[ClassTag[T]], parent)
    repository.put(id, Box(a))
    nodeRefs.put(scala.Symbol(e.name), a)
    id
  }
  override def buildOptNode[T: ClassTag](entity: Int,
                                         value: Seq[String],
                                         parent: scala.Symbol): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[OptionArg[T] with ArgValue[T]](entity)
    val a =
      OptNode[T](entity = e, value = value, tpe = implicitly[ClassTag[T]], parent: scala.Symbol)
    repository.put(id, Box(a))
    nodeRefs.put(scala.Symbol(e.name), a)
    id
  }
  override def buildPropNode[T: ClassTag](entity: Int,
                                          value: Seq[(String, String)]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[PropertyArg[T] with VariableValue[(String, T)]](entity)
    val a = PropNode(entity = e, value = value, tpe = implicitly[ClassTag[T]])
    repository.put(id, Box(a))
    nodeRefs.put(scala.Symbol(e.name), a)
    id
  }
  override def buildPriorNode(entity: Int, parent: scala.Symbol): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[PriorArg](entity)
    val a = PriorNode(e, parent)
    repository.put(id, Box(a))
    nodeRefs.put(scala.Symbol(e.name), a)
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
  override def buildCmdNode(entity: Int,
                            params: Seq[Int],
                            opts: Seq[Int],
                            priors: Seq[Int],
                            subCmdEntry: Int,
                            limitations: Seq[(MutualLimitation, Seq[scala.Symbol])]): Int = {
    val id = idGen.getAndIncrement()
    val e = getEntity[Command](entity)
    val p = params.map(getEntity[ParamNode[_]])
    val o = opts.map(getEntity[OptNode[_]])
    val pr = priors.map(getEntity[PriorNode])
    val se = getEntity[CmdEntryNode](subCmdEntry)
    val a = CmdNode(e, p, o, pr, se, limitations)
    repository.put(id, Box(a))
    nodeRefs.put(scala.Symbol(e.name), a)
    id
  }
  override def buildArgTree(appName: String,
                            topParams: Seq[Int],
                            topOpts: Seq[Int],
                            topPriors: Seq[Int],
                            props: Seq[Int],
                            cmdEntry: Int,
                            topLimitations: Seq[(MutualLimitation, Seq[scala.Symbol])],
                            globalLimitations: Seq[(MutualLimitation, Seq[scala.Symbol])]): this.type = {
    val tp = topParams.map(getEntity[ParamNode[_]])
    val to = topOpts.map(getEntity[OptNode[_]])
    val tpr = topPriors.map(getEntity[PriorNode])
    val ps = props.map(getEntity[PropNode[_]])
    val ce = getEntity[CmdEntryNode](cmdEntry)
    argTree = Some(ArgTree(appName, tp, to, tpr, ps, ce, topLimitations, globalLimitations))
    repository.clear()
    this
  }
  override def addValidation[T](name: String, func: T => Unit): Unit = { //todo:move scala.Symbol to compile time
    nodeRefs.get(scala.Symbol(name)) match {
      case Some(node: ValueNode[T@unchecked]) => valiRefs.put(node, func)
      case Some(node: PropNode[T@unchecked]) => valiRefs.put(node, func)
      case Some(node) =>
        throw new AssertionError(s"Node[$node] with name: $name is not compatible with validation.")
      case None => throw new AssertionError(s"Cannot find node with name: $name")
    }
  }
  override def parse(args: Seq[String]): Seq[String] = {
    if (parsedNodes.nonEmpty) throw new IllegalStateException("ScmdRuntime cannot parse args twice.")
    val parsedResults = ArgParser.parse(argTree, args)
    val validated = Validator.mutualLimitationValidate(argTree, parsedResults)
    validated.map {
      case (node, cs) =>
        parsedNodes.put(scala.Symbol(node.entity.name), node)
        parsedContextSnapshots.put(node, cs)
    }
    parsedNodes.keys.toSeq.map(_.name)
  }

  /** Return not parsed node. */
  private def getNodeByName[N <: Node : ClassTag](name: scala.Symbol): N = {
    this.getNode[N](name, nodeRefs) match {
      case Some(n) => n
      case None =>
        throw new NoSuchElementException(s"Node of name:$name cannot be found in runtime.")
    }
  }

  /** (private) Given name and type, query a node from a provided map. */
  private def getNode[N <: Node : ClassTag](name: scala.Symbol,
                                            refs: mutable.Map[scala.Symbol, Node]): Option[N] = {
    val tpe = implicitly[ClassTag[N]].runtimeClass
    val node: Option[Node] = refs.get(name).map {
      case cmdNode: CmdNode if tpe == classOf[CmdNode] => cmdNode
      case paramNode: ParamNode[_]
        if tpe == classOf[ParamNode[_]] || tpe == classOf[ValueNode[_]] => paramNode
      case optNode: OptNode[_]
        if tpe == classOf[OptNode[_]] || tpe == classOf[ValueNode[_]] => optNode
      case propNode: PropNode[_] if tpe == classOf[PropNode[_]] => propNode
      case priorNode: PriorNode if tpe == classOf[PriorNode] => priorNode
      case _: CmdEntryNode =>
        throw new AssertionError(s"CmdEntryNode has no name and should not be cached.")
      case bad =>
        throw new MatchError(s"Node of specified type[$tpe] has not specified name:$name.$bad")
    }
    node.map(_.asInstanceOf[N])
  }

  override def getEvaluatedArgumentByName
  [T: ClassTag : ArgTypeEvidence, A <: Argument[T] : ClassTag](name: scala.Symbol): A = {
    if (parsedNodes.isEmpty) throw new AssertionError("Parsed node empty before query by name.")
    val valueTpe = implicitly[ClassTag[T]]
    val argTpe = implicitly[ClassTag[A]]

    /** Get parsed node by type. */
    def parsedNode[N <: Node : ClassTag]: Option[N] = this.getNode[N](name, parsedNodes)
    def checkNodeType(n: {def tpe: ClassTag[_]}): Unit = if (n.tpe != valueTpe)
      throw new AssertionError(s"Node's value type[${n.tpe}]" +
        s" is different from specified type[$valueTpe].")
    def getCS(n: Node): ContextSnapshot = parsedContextSnapshots
      .getOrElse(n, throw new AssertionError("ContextSnapshot should company parsed node. "))

    val argument: Argument[_] = argTpe.runtimeClass match {
      case rc if rc == classOf[Command] =>
        val node = this.getNodeByName[CmdNode](name)
        node.entity.copy(met = parsedNodes.get(name).nonEmpty)

      case rc if rc == classOf[Parameter[_]] || rc == classOf[OptionArg[_]] =>
        val node = this.getNodeByName[ValueNode[_]](name)
        checkNodeType(node)
        val value: Seq[T] = parsedNode[ValueNode[T]] match {
          case None => Seq.empty[T]
          case Some(n) => Validator.validateValueNode(n, getCS(n), valiRefs.get(n))
        }
        merge(node.entity.asInstanceOf[ValueArgument[T]], value)

      case rc if rc == classOf[PropertyArg[_]] =>
        val node = this.getNodeByName[PropNode[_]](name)
        checkNodeType(node)
        val value: Seq[(String, T)] = parsedNode[PropNode[T]] match {
          case None => Seq.empty
          case Some(n) => Validator.validatePropNode(n, getCS(n), valiRefs.get(n))
        }
        merge(node.entity.asInstanceOf[PropertyArg[T]], value)

      case rc if rc == classOf[PriorArg] =>
        val node = this.getNodeByName[PriorNode](name)
        node.entity.copy(met = parsedNode[PriorNode].map(_.parent))
    }
    argument.asInstanceOf[A]
  }
  override def runBuiltInPriors(): Unit = {
    //todo: implementation
  }
  override def handleException[E <: ScmdException : ScmdExceptionHandler](e: E): Nothing = {
    implicitly[ScmdExceptionHandler[E]].handle(e)
  }
  override def clean(): Unit = {
    appInfo = None
    argTree = None
    repository.clear()
    nodeRefs.clear()
    valiRefs.clear()
    parsedNodes.clear()
    parsedContextSnapshots.clear()
  }

  override def argTreeString: String = useArgTree(argTree).prettyString
  override def appInfoString: String = useAppInfo(appInfo).prettyString
  override def parsedSeqString: String = parsedNodes.values.toSeq.prettyString

  private implicit def useArgTree(argTreeOpt: Option[ArgTree]): ArgTree =
    argTreeOpt.getOrElse(throw new IllegalStateException("argTree not initialized."))
  private implicit def useAppInfo(appInfoOpt: Option[AppInfo]): AppInfo =
    appInfoOpt.getOrElse(throw new IllegalStateException("appInfo not initialized."))

}