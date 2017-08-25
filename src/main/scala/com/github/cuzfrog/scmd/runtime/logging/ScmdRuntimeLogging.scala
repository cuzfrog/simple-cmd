package com.github.cuzfrog.scmd.runtime.logging

import com.github.cuzfrog.scmd.{ArgValue, Argument, SingleValue, VariableValue}
import com.github.cuzfrog.scmd.internal.{IgnoreLogging, SimpleLogging}
import com.github.cuzfrog.scmd.runtime.{ArgTypeEvidence, Node, ScmdRuntime, ScmdRuntimeImpl, ValueNode}

import scala.reflect.ClassTag

private[runtime] trait ScmdRuntimeLogging extends ScmdRuntimeImpl with SimpleLogging {
  override protected lazy val loggerAgent: SimpleLogging.LoggerAgent = classOf[ScmdRuntime].getName

  @IgnoreLogging
  abstract override def buildParamNode[T: ClassTag](entity: Int,
                                                    value: Seq[String],
                                                    parent: scala.Symbol): Int = {
    debug(s"Build ParamNode[${implicitly[ClassTag[T]]}]")
    super.buildParamNode[T](entity, value, parent)
  }
  @IgnoreLogging
  abstract override def buildCmdEntry(isMandatory: Boolean): Int = {
    val result = super.buildCmdEntry(isMandatory)
    debug(s"Build cmd entry id:$result")
    result
  }

  @IgnoreLogging
  abstract override def addValidation[T](name: scala.Symbol, func: (T) => Unit): Unit = {
    debug(s"Add validation for arg:$name")
    super.addValidation(name, func)
  }
  @IgnoreLogging
  abstract override def getEvaluatedArgumentByName
  [T: ClassTag : ArgTypeEvidence, A <: Argument[T] : ClassTag](name: scala.Symbol): A = {
    val result = super.getEvaluatedArgumentByName[T, A](name)
    val value = result match {
      case v: SingleValue[_] => v.v.toSeq
      case v: VariableValue[_] => v.v
      case v => Nil
    }
    debug(s"Get evaluated Argument(${result.name})" +
      s" of type[${implicitly[ClassTag[T]]}]] by name:$name | value: $value")
    result
  }


  abstract override def parse(args: Seq[String]): Seq[String] = {
    val result = super.parse(args)
    debug(s"Try to parse arguments:${args.mkString(" ")}, result: ${result.mkString(",")}")
    result
  }
}
