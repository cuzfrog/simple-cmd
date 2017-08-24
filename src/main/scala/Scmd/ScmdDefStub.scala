package Scmd

import com.github.cuzfrog.scmd.runtime.console.ConsoleType
import com.github.cuzfrog.scmd.{PriorArg, ScmdDefApi}

/**
  * Api stub for argument def-class.
  *
  * @tparam D the type of argument def-class
  */
abstract class ScmdDefStub[D] extends ScmdDefApi {
  /**
    * Add validation to this def-class.
    *
    * @param vali function that accepts def-class and return validation class.
    * @tparam T validation class.
    * @return the same def-class added with validation.
    */
  def withValidation[T](vali: D => T): this.type = Empty
  /**
    * Provide route to define running of this def-class.
    *
    * @param toRoute function that accepts def-class and return [[ArgRoute]]
    * @return true if the route has been run, false if route has been passed through.<br>
    * @see [[com.github.cuzfrog.scmd.RouteCommandOperations]]
    */
  def runWithRoute(toRoute: D => ArgRoute): Boolean = Empty
  /**
    * Parse against arguments.
    *
    * @return evaluated def-class.
    */
  def parse: D = Empty
  def defaultUsageString(implicit consoleType: ConsoleType): String = Empty
  /** Built-in priorArg. */
  val help: PriorArg = Empty
  /** Built-in priorArg. */
  val version: PriorArg = Empty

  private def Empty[T]: T =
    throw new AssertionError("Methods in ScmdDefStub are dummy, they should not be called directly." +
      "Have you added @ScmdDef to this class?")
}
