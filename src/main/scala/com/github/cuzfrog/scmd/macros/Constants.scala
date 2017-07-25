package com.github.cuzfrog.scmd.macros

import scala.meta._

private[macros] object Constants {
  val TERM_NAME = q"name"
  val TERM_DESCRIPTION = q"description"
  val TERM_ABBREVIATION = q"abbr"
  val TERM_IS_MANDATORY = q"isMandatory"
  val TERM_SUBCMDS = q"subCmds"
  val TERM_immutable = q"_root_.scala.collection.immutable"
  val TERM_pkg_scmd = q"com.github.cuzfrog.scmd"
  val TERM_RUNTIME = q"com.github.cuzfrog.scmd.runtime.ScmdRuntime"

  val TYPE_NOTHING = t"Nothing"
}
