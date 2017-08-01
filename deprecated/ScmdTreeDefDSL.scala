package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.runtime.ScmdRuntime

@deprecated("Parse dsl at compile time through macros, this should be made dummy.","dev")
object ScmdTreeDefDSL {

  /** Entry to define g argument tree. */
  final def argTreeDef(subArg: ArgGroup, moreSubArg: ArgGroup*)
                      (implicit runtime: ScmdRuntime): Unit = {

  }

  implicit final class CommandTreeDefOps(a: Command) {
    def apply(subArg: ArgGroup, moreSubArg: ArgGroup*): CmdGroup = {
      new CmdGroup(a, subArg +: moreSubArg)
    }
  }

  //implicit final def valueArgument2Group(a: ValueArgument[_]): ArgGroup = new ArgGroup(a, Seq())

  implicit final class ValueArgumentOps(a: ValueArgument[_]) {
    def |(that: ValueArgument[_]): ValueGroup with MutuallyExclusive = {
      new ValueGroup(Seq(a, that)) with MutuallyExclusive
    }

    def &(that: ValueArgument[_]): ValueGroup with MutuallyDependent = {
      new ValueGroup(Seq(a, that)) with MutuallyDependent
    }
  }

  implicit final class MutuallyExclusiveOps(g: ValueGroup with MutuallyExclusive) {
    /** One of (mutually exclusive) */
    def |(that: ValueArgument[_]): ValueGroup with MutuallyExclusive = {
      new ValueGroup(g.valueArgs :+ that) with MutuallyExclusive
    }
  }

  implicit final class MutuallyDependentOps(g: ValueGroup with MutuallyDependent) {
    /** Both of (mutually inclusive) */
    def &(that: ValueArgument[_]): ValueGroup with MutuallyDependent = {
      new ValueGroup(g.valueArgs :+ that) with MutuallyDependent
    }
  }

  sealed trait ArgGroup
  sealed case class CmdGroup private[scmd](cmd: Command, subArgs: Seq[ArgGroup]) extends ArgGroup
  sealed case class ValueGroup private[scmd](valueArgs: Seq[ValueArgument[_]]) extends ArgGroup
  sealed case class ArgBox private[scmd](entity: Argument[_]) extends ArgGroup
  private object ArgBox {
    implicit def box(entity: Argument[_]): ArgBox = ArgBox(entity)
  }
  sealed trait MutuallyExclusive
  sealed trait MutuallyDependent
}
