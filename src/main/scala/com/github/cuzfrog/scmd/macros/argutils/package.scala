package com.github.cuzfrog.scmd.macros

import scala.meta.{Pat, Term}

package object argutils {
  private[argutils] implicit class String2PatTermOps(in: String){
    def toPatTerm: Pat.Var.Term = Pat.Var.Term(Term.Name(in)).asInstanceOf[Pat.Var.Term]
  }

}
