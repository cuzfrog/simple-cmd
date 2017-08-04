package com.github.cuzfrog.scmd.runtime

class
ArgParseException private(val msg: String,
                          private[runtime] val contextSnapshot: ContextSnapshot)
  extends RuntimeException(msg) {

}

private object ArgParseException {
  def apply(msg: String,
            contextSnapshot: ContextSnapshot): ArgParseException =
    new ArgParseException(msg, contextSnapshot)

  implicit def toLeft[R](argParseException: ArgParseException): Either[ArgParseException, R] =
    Left(argParseException)
}

class
ArgValidationException private(val msg: String,
                               private[runtime] val contextSnapshot: ContextSnapshot,
                               val cause: Option[Exception] = None)
  extends RuntimeException(msg)

private object ArgValidationException {
  def apply(msg: String,
            contextSnapshot: ContextSnapshot,
            cause: Option[Exception] = None): ArgValidationException =
    new ArgValidationException(msg, contextSnapshot, cause)
}