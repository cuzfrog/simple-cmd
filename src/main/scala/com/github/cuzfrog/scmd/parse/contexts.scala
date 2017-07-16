package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{CmdNode, ValueNode}

case class Scope(cmdNode: CmdNode, argCursor: Int)
case class ValueAnchor(valueNode: ValueNode, scope: Scope)
case class ArgParseException(msg: String, scope: Scope) extends RuntimeException(msg)


object ArgParseException {
  implicit def toLeft[R](argParseException: ArgParseException): Either[ArgParseException, R] =
    Left(argParseException)
}