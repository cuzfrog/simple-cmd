package com.github.cuzfrog.scmd.parse

trait ArgTypeEvidence[V] {
  def verify(v: Seq[String]): V
}

object ArgTypeEvidence{

}