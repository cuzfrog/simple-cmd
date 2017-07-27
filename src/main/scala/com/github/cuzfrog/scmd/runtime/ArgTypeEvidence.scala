package com.github.cuzfrog.scmd.runtime

trait ArgTypeEvidence[V] {
  def verify(v: String): V
}

object ArgTypeEvidence{

}