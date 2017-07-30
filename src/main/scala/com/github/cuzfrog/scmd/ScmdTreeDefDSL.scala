package com.github.cuzfrog.scmd

object ScmdTreeDefDSL {

  /** Entry to define a argument tree. */
  final def argTreeDef(subArg: Argument[_], moreSubArg: Argument[_]*): Unit = ()

  implicit final class CommandTreeDefOps(a: Command) {
    def apply(subArg: Argument[_], moreSubArg: Argument[_]*): Command = a
  }

  implicit final class ArgumentTreeDefOps(a: ValueArgument[_]) {
    /** One of (mutually exclusive) */
    def |(that: ValueArgument[_]): ValueArgument[_] with MutuallyExclusive =
      a.asInstanceOf[ValueArgument[_] with MutuallyExclusive]

    /** Both of (mutually inclusive) */
    def &(that: ValueArgument[_]): ValueArgument[_] with MutuallyInclusive =
      a.asInstanceOf[ValueArgument[_] with MutuallyInclusive]
  }

  implicit final class MutuallyExclusiveOps(a: ValueArgument[_] with MutuallyExclusive) {
    /** One of (mutually exclusive) */
    def |(that: ValueArgument[_]): ValueArgument[_] with MutuallyExclusive = a
  }

  implicit final class MutuallyInclusiveOps(a: ValueArgument[_] with MutuallyInclusive) {
    /** Both of (mutually inclusive) */
    def &(that: ValueArgument[_]): ValueArgument[_] with MutuallyInclusive = a
  }

  sealed trait MutuallyExclusive
  sealed trait MutuallyInclusive
}
