package com.github.cuzfrog.scmd.runtime

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

trait ArgTypeEvidence[V] {
  def verify(v: String): V
}

object ArgTypeEvidence {
  implicit val nothingEv:ArgTypeEvidence[Nothing] = (v:String) =>
    throw new UnsupportedOperationException("ArgTypeEvidence[Nothing] should not be used.")

  implicit val stringEv: ArgTypeEvidence[String] = (v: String) => v
  implicit val booleanEv: ArgTypeEvidence[Boolean] = (v: String) => v.toBoolean
  implicit val intEv: ArgTypeEvidence[Int] = (v: String) => v.toInt
  implicit val longEv: ArgTypeEvidence[Long] = (v: String) => v.toLong
  implicit val floatEv: ArgTypeEvidence[Float] = (v: String) => v.toFloat
  implicit val doubleEv: ArgTypeEvidence[Double] = (v: String) => v.toDouble

  implicit val pathEv: ArgTypeEvidence[Path] = (v: String) => Paths.get(v)
  implicit val fileEv: ArgTypeEvidence[File] = (v: String) => new File(v)

}