package com.github.cuzfrog.scmd.runtime

private[runtime] case
class ArgParseException(msg: String,
                        contextSnapshot: ContextSnapshot,
                        cause: Option[Exception] = None) extends RuntimeException(msg)

private object ArgParseException {
  implicit def toLeft[R](argParseException: ArgParseException): Either[ArgParseException, R] =
    Left(argParseException)
}