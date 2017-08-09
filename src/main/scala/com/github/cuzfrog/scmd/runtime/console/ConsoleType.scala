package com.github.cuzfrog.scmd.runtime.console

sealed trait ConsoleType
private[runtime] object ConsoleType {
  def detect: ConsoleType = new ConsoleType {} //todo: detect console type.
}




