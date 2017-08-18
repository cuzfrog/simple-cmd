package com.github.cuzfrog.scmd.runtime


sealed abstract class ScmdException(val msg: String) extends Exception(msg)

class ArgParseException private(override val msg: String,
                                private[runtime] val contextSnapshot: ContextSnapshot)
  extends ScmdException(msg) {

}

private object ArgParseException {
  def apply(msg: String,
            contextSnapshot: ContextSnapshot): ArgParseException =
    new ArgParseException(msg, contextSnapshot)

  implicit def toLeft[R](argParseException: ArgParseException): Either[ArgParseException, R] =
    Left(argParseException)
}

class ArgValidationException private(override val msg: String,
                                     private[runtime] val contextSnapshot: ContextSnapshot,
                                     val cause: Option[Exception] = None)
  extends ScmdException(msg)

private object ArgValidationException {
  def apply(msg: String,
            contextSnapshot: ContextSnapshot,
            cause: Option[Exception] = None): ArgValidationException =
    new ArgValidationException(msg, contextSnapshot, cause)
}

trait ScmdExceptionHandler[E <: ScmdException] {
  def handle(e: E): Nothing
}

private object ScmdExceptionHandler {
  implicit val defaultParseExceptionHandler: ScmdExceptionHandler[ArgParseException] =
    (e: ArgParseException) => {
      System.err.println(s"error: ${e.getMessage}" + NEWLINE + "use '-help' to get usage info.")
      throw e
    }
  //todo: provide client api to get error detail.
  implicit val defaultValidationExceptionHandler: ScmdExceptionHandler[ArgValidationException] =
    (e: ArgValidationException) => {
      System.err.println(s"error: ${e.getMessage}" + NEWLINE + "use '-help' to get usage info.")
      throw e
    }

  /** Client provides this to override default handling behavior. */
  implicit val defaultHandler: ScmdExceptionHandler[ScmdException] = {
    case ex: ArgParseException => defaultParseExceptionHandler.handle(ex)
    case ex: ArgValidationException => defaultValidationExceptionHandler.handle(ex)
  }
}