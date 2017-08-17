package com.github.cuzfrog.scmd

import Scmd.ScmdDefStub
import com.github.cuzfrog.scmd.runtime.ScmdRuntime

abstract class ScmdDefTestStub[D] extends ScmdDefStub[D] {
  def argTreeString: String
  def getRuntime: ScmdRuntime
}
