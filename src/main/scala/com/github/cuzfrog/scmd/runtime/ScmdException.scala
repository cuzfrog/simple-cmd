package com.github.cuzfrog.scmd.runtime


sealed abstract class ScmdException(val msg: String) extends Exception(msg)

class ArgParseException private(override val msg: String,
                                val code: ScmdExceptionCode,
                                private[runtime] val contextSnapshot: ContextSnapshot)
  extends ScmdException(msg) {

}

private object ArgParseException {
  def apply(msg: String,
            code: ScmdExceptionCode,
            contextSnapshot: ContextSnapshot): ArgParseException =
    new ArgParseException(msg, code, contextSnapshot)

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
    new ScmdExceptionHandler[ArgParseException] {
      override def handle(e: ArgParseException): Nothing = {
        val hint = e.code match {
          case ScmdExceptionCode.UNDEFINED_ARGS(Some(correction)) => s"did you mean: '$correction'?"
          case _ => "use '-help' to get usage info."
        }
        System.err.println(s"error: ${e.getMessage}" + NEWLINE + hint)
        throw e
      }
    }
  //todo: provide client api to get error detail.
  implicit val defaultValidationExceptionHandler: ScmdExceptionHandler[ArgValidationException] =
    new ScmdExceptionHandler[ArgValidationException] {
      override def handle(e: ArgValidationException): Nothing = {
        System.err.println(s"error: ${e.getMessage}" + NEWLINE + "use '-help' to get usage info.")
        throw e
      }
    }

  /** Client provides this to override default handling behavior. */
  implicit val defaultHandler: ScmdExceptionHandler[ScmdException] =
    new ScmdExceptionHandler[ScmdException] {
      override def handle(e: ScmdException): Nothing = e match {
        case ex: ArgParseException => defaultParseExceptionHandler.handle(ex)
        case ex: ArgValidationException => defaultValidationExceptionHandler.handle(ex)
      }
    }
}

sealed trait ScmdExceptionCode
object ScmdExceptionCode {
  /** No args supplied from the cmd-lin. */
  case object NO_ARGS extends ScmdExceptionCode
  case object AMBIGUOUS_ARGS extends ScmdExceptionCode
  /** More cmd-line args required to satisfy defined args. */
  case object DEFICIT_ARGS extends ScmdExceptionCode
  /** Unmatched args, which is tried to be corrected. */
  case class UNDEFINED_ARGS(correction: Option[String] = None) extends ScmdExceptionCode
  case object DUPLICATE_ARGS extends ScmdExceptionCode
  /** Arg with bad format that violates parsing rules. */
  case object BAD_FORMAT extends ScmdExceptionCode
  /** Literals that should be parsed as boolean(true/false), but cannot be. */
  case object BAD_BOOLEAN_LITERAL extends ScmdExceptionCode
}
