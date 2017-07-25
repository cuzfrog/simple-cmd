package com.github.cuzfrog.scmd.runtime

trait ArgTypeEvidence[V] {
  def verify(v: Seq[String]): V
}

object ArgTypeEvidence{

}