package com.github.cuzfrog.scmd.runtime.console

sealed trait ConsoleType
object ConsoleType {
  implicit def detect: ConsoleType = new ConsoleType {}
  //todo: detect console type.
}




