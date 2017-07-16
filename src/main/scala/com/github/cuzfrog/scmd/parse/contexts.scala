package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{CmdNode, ValueNode}

case class Scope(cmdNode: CmdNode)
case class ValueAnchor(belong: CmdNode, valueNode: ValueNode[_])
case class ArgParseException(msg: String, scope: Scope) extends RuntimeException(msg)


