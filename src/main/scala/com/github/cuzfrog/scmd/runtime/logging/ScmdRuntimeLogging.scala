package com.github.cuzfrog.scmd.runtime.logging

import com.github.cuzfrog.scmd.ArgValue
import com.github.cuzfrog.scmd.internal.{IgnoreLogging, SimpleLogger}
import com.github.cuzfrog.scmd.runtime.{ScmdRuntime, ScmdRuntimeImpl}

import scala.reflect.ClassTag

private[runtime] trait ScmdRuntimeLogging extends ScmdRuntimeImpl with SimpleLogger {
  override val loggerAgent = classOf[ScmdRuntime].getName

  @IgnoreLogging
  abstract override def buildParamNode[T: ClassTag](entity: Int, value: Seq[String]): Int = {
    debug(s"Buil ParamNode[${implicitly[ClassTag[T]]}]")
    super.buildParamNode[T](entity, value)
  }
}
