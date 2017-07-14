package com.github.cuzfrog.scmd.macros

import scala.meta._

private object Constents {
  val TERM_NAME = Term.Name("name")
  val TERM_DESCRIPTION = Term.Name("description")
  val TERM_IS_MANDATORY = Term.Name("isMandatory")
  val TERM_immutable = q"_root_.scala.collection.immutable"

  val TYPE_NOTHING = Type.Name("Nothing")
}
