package com.github.cuzfrog.scmd.runtime.console

import com.github.cuzfrog.scmd.runtime.NEWLINE

private final class BuilderHelper(val v: String) {
  var condition: Boolean = true
}
private object BuilderHelper {
  implicit def fromString(str: String): BuilderHelper = new BuilderHelper(str)
}

private trait BuilderUtils {

  protected implicit class ExStringOps[S](in: S)
                                         (implicit builder: StringBuilder,
                                          conversion: S => BuilderHelper) {
    val bh: BuilderHelper = in match {
      case b: BuilderHelper => b
      case s: S@unchecked => conversion(in)
    }

    @inline def add(bracket: Boolean = false): BuilderHelper = {
      if (bh.condition) {
        if (bracket) builder.append('[')
        builder.append(bh.v)
        if (bracket) builder.append(']')
      }
      bh
    }
    @inline def line: BuilderHelper = {
      if (bh.condition) {
        builder.append(bh.v).append(NEWLINE)
      }
      bh
    }
    @inline def space: BuilderHelper = {
      if (bh.condition) {
        builder.append(' ')
      }
      bh
    }
    @inline def indent(n: Int): BuilderHelper = {
      if (bh.condition) {
        var i: Int = 0
        while (i < n) {
          builder.append(' ')
          i += 1
        }
      }
      bh
    }
    @inline def condition(on: Boolean): BuilderHelper = {
      bh.condition = on
      bh
    }
  }
}
