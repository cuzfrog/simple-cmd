package com.github.cuzfrog.scmd.runtime.console


private[console] case class Indent(v: Int) extends AnyVal {
  def +(that: Indent): Indent = Indent(v + that.v)
}
private[console] object Indent {
  implicit def fromInt(i: Int): Indent = Indent(i)
  implicit def toInt(in: Indent): Int = in.v
}
