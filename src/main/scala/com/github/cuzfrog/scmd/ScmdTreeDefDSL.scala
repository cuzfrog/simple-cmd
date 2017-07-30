package com.github.cuzfrog.scmd

object ScmdTreeDefDSL {

  implicit class CommandTreeDefOps(a: Command) {
    def apply(subArg: Argument[_], moreSubArg: Argument[_]*): Command = a
  }

  implicit class ArgumentTreeDefOps[A, T](a: A)(implicit ev: A <:< Argument[T]) {
    /** One of (mutually excluded) */
    def |(that: Argument[_]): A = a

    /** Both of */
    def &(that: Argument[_]): A = a
  }
}
