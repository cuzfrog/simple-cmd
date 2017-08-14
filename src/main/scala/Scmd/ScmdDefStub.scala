package Scmd

import com.github.cuzfrog.scmd.runtime.console.ConsoleType
import com.github.cuzfrog.scmd.{ArgRoute, PriorArg, ScmdDefApi}


abstract class ScmdDefStub[D] extends ScmdDefApi {
  // ------------- stub methods -----------------
  def withValidation[T](vali: D => T): this.type
  def runWithRoute(toRoute: D => ArgRoute): Boolean
  def parse: D
  def defaultUsageString(implicit consoleType: ConsoleType): String
  val help: PriorArg
  val version: PriorArg
}
