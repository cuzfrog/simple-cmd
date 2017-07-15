package com.github.cuzfrog.scmd.macros

import scala.meta._

private object Constants {
  val TERM_NAME = q"name"
  val TERM_DESCRIPTION = q"description"
  val TERM_ABBREVIATION = q"abbr"
  val TERM_IS_MANDATORY = q"isMandatory"
  val TERM_immutable = q"_root_.scala.collection.immutable"

  val TYPE_NOTHING = t"Nothing"
}
