package com.github.cuzfrog.scmd

import scala.meta.Term

package object macros {
  private[macros] trait Definable[A] {
    def defnRuntimeTerm(a: A): Term
  }

  private[macros] implicit class DefineTermOps[A: Definable](a: A) {
    def defnRuntimeTerm: Term = {
      val definable = implicitly[Definable[A]]
      definable.defnRuntimeTerm(a)
    }
  }
}
