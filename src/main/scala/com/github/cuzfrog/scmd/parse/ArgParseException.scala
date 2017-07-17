package com.github.cuzfrog.scmd.parse

private case class ArgParseException(msg: String, scope: Context) extends RuntimeException(msg)
private object ArgParseException {
  implicit def toLeft[R](argParseException: ArgParseException): Either[ArgParseException, R] =
    Left(argParseException)
}
