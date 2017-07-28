package com.github.cuzfrog.scmd.runtime.logging

import com.github.cuzfrog.scmd.{ArgValue, Argument}
import com.github.cuzfrog.scmd.internal.{IgnoreLogging, SimpleLogging}
import com.github.cuzfrog.scmd.runtime.{ArgTypeEvidence, Node, ScmdRuntime, ScmdRuntimeImpl, ValueNode}

import scala.reflect.ClassTag

private[runtime] trait ScmdRuntimeLogging extends ScmdRuntimeImpl with SimpleLogging {
  override val loggerAgent = classOf[ScmdRuntime].getName

  @IgnoreLogging
  abstract override def buildParamNode[T: ClassTag](entity: Int, value: Seq[String]): Int = {
    debug(s"Build ParamNode[${implicitly[ClassTag[T]]}]")
    super.buildParamNode[T](entity, value)
  }
  @IgnoreLogging
  abstract override def addValidation[T](name: String, func: (T) => Unit): Unit = {
    debug(s"Add validation for arg:$name, func:$func")
    super.addValidation(name, func)
  }
  @IgnoreLogging
  abstract override def getEvaluatedArgumentByName
  [T: ClassTag : ArgTypeEvidence, A <: Argument[T] : ClassTag](name: String): A = {
    val result = super.getEvaluatedArgumentByName[T, A](name)
    debug(s"Get evaluated Argument(${result.name}) of type[${implicitly[ClassTag[T]]}]] by name:$name")
    result
  }

  @IgnoreLogging
  abstract override def parse(args: Seq[String]): Seq[String] = {
    val result = super.parse(args)
    debug(s"Try to parse arguments:${args.mkString(" ")}, result:${result.mkString(",")}")
    result
  }
}
