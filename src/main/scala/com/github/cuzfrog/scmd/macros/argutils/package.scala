package com.github.cuzfrog.scmd.macros

import scala.meta.{Pat, Term}

package object argutils {
  private[argtuils] implicit def string2PatTerm(in: String): Pat.Var.Term = Pat.Var.Term(Term.Name(in))
}
