package com.github.cuzfrog.scmd.parse

trait ArgTypeEvidence[V] {
  def verify(v: String): V
}

object ArgTypeEvidence{

}