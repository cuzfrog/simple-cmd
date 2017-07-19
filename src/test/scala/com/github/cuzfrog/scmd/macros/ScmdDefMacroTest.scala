package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._

/**
  * Created by cuz on 7/19/17.
  */
private[scmd] object ScmdDefMacroTest extends ScmdDefMacro{
  override protected val isTestMode = true
}
