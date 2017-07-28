package com.github.cuzfrog.scmd.runtime

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

trait ArgTypeEvidence[V] {
  def verify(v: String): V
}

object ArgTypeEvidence {
  implicit val stringEv: ArgTypeEvidence[String] = (v: String) => v
  implicit val booleanEv: ArgTypeEvidence[Boolean] = (v: String) => v.toBoolean


  implicit val pathEv: ArgTypeEvidence[Path] = (v: String) => Paths.get(v)
  implicit val fileEv: ArgTypeEvidence[File] = (v: String) => new File(v)
}