package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.{OptionArg, Parameter, SingleValue, VariableValue}

import scala.meta._

private object Types {
  val parameter = Type.Name(classOf[Parameter[_]].getSimpleName)
  val optionArg = Type.Name(classOf[OptionArg[_]].getSimpleName)

  val variableValue = Type.Name(classOf[VariableValue[_]].getSimpleName)
  val singleValue = Type.Name(classOf[SingleValue[_]].getSimpleName)
}