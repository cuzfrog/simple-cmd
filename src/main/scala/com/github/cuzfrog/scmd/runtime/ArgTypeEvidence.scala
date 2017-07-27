package com.github.cuzfrog.scmd.runtime

trait ArgTypeEvidence[V] {
  def verify(v: String): V
}

object ArgTypeEvidence {
  implicit val stringEv: ArgTypeEvidence[String] = (v: String) => v
  implicit val booleanEv: ArgTypeEvidence[Boolean] = (v: String) => v.toBoolean
}