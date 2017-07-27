package com.github.cuzfrog.scmd.runtime.logging

import com.github.cuzfrog.scmd.{ArgValue, Argument}
import com.github.cuzfrog.scmd.internal.{IgnoreLogging, SimpleLogger}
import com.github.cuzfrog.scmd.runtime.{ArgTypeEvidence, ScmdRuntime, ScmdRuntimeImpl}

import scala.reflect.ClassTag

private[runtime] trait ScmdRuntimeLogging extends ScmdRuntimeImpl with SimpleLogger {
  override val loggerAgent = classOf[ScmdRuntime].getName

  @IgnoreLogging
  abstract override def buildParamNode[T: ClassTag](entity: Int, value: Seq[String]): Int = {
    debug(s"Build ParamNode[${implicitly[ClassTag[T]]}]")
    super.buildParamNode[T](entity, value)
  }

  abstract override def getArgumentWithValueByName
  [T: ClassTag : ArgTypeEvidence, A <: Argument[T] : ClassTag](name: String): A = {
    val result = super.getArgumentWithValueByName[T, A](name)
    debug(s"Get evaluated Argument(${result.name}) of type[${implicitly[ClassTag[T]]}]] by name:$name")
    result
  }
}
