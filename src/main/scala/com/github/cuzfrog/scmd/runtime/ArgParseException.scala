package com.github.cuzfrog.scmd.runtime

import scala.reflect.ClassTag

sealed abstract class ScmdException(val msg: String) extends Exception(msg)

class
ArgParseException private(override val msg: String,
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

class
ArgValidationException private(override val msg: String,
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
  def handle(e: E): Unit
}

private object ScmdExceptionHandler {
  implicit val defaultParseExceptionHandler: ScmdExceptionHandler[ArgParseException] =
    (e: ArgParseException) => {
      println(s"Handle parse exception: ${e.getMessage}")
    }

  implicit val defaultValidationExceptionHandler: ScmdExceptionHandler[ArgValidationException] =
    (e: ArgValidationException) => {
      println(s"Handle validation exception: ${e.getMessage}")
    }

  implicit val defaultHandler: ScmdExceptionHandler[ScmdException] = {
    case ex: ArgParseException => defaultParseExceptionHandler.handle(ex)
    case ex: ArgValidationException => defaultValidationExceptionHandler.handle(ex)
  }
}