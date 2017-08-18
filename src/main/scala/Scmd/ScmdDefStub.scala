package Scmd

import com.github.cuzfrog.scmd.runtime.console.ConsoleType
import com.github.cuzfrog.scmd.{PriorArg, ScmdDefApi}


abstract class ScmdDefStub[D] extends ScmdDefApi {
  // ------------- stub methods -----------------

  /**
    * Add validation to this def-class.
    *
    * @param vali function that accepts def-class and return validation class.
    * @tparam T validation class.
    * @return the same def-class added with validation.
    */
  def withValidation[T](vali: D => T): this.type
  /**
    * Provide route to define running of this def-class.
    *
    * @param toRoute function that accepts def-class and return [[ArgRoute]]
    * @return true if the route has been run, false if route has been passed through.<br>
    * @see [[com.github.cuzfrog.scmd.RouteCommandOperations]]
    */
  def runWithRoute(toRoute: D => ArgRoute): Boolean
  /**
    * Parse against arguments.
    *
    * @return evaluated def-class.
    */
  def parse: D
  def defaultUsageString(implicit consoleType: ConsoleType): String
  /** Built-in priorArg. */
  val help: PriorArg
  /** Built-in priorArg. */
  val version: PriorArg
}
