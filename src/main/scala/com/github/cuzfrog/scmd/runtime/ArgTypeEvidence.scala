package com.github.cuzfrog.scmd.runtime

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

trait ArgTypeEvidence[V] {
  def verify(v: String): V
}

object ArgTypeEvidence {
  implicit val nothingEv: ArgTypeEvidence[Nothing] =
    new ArgTypeEvidence[Nothing] {
      override def verify(v: String): Nothing =
        throw new UnsupportedOperationException("ArgTypeEvidence[Nothing] should not be used.")
    }

  implicit val stringEv: ArgTypeEvidence[String] =
    new ArgTypeEvidence[String] {
      override def verify(v: String): String = v
    }
  implicit val booleanEv: ArgTypeEvidence[Boolean] =
    new ArgTypeEvidence[Boolean] {
      override def verify(v: String): Boolean = v.toBoolean
    }
  implicit val intEv: ArgTypeEvidence[Int] =
    new ArgTypeEvidence[Int]{
      override def verify(v: String): Int =  v.toInt
    }
  implicit val longEv: ArgTypeEvidence[Long] =
    new ArgTypeEvidence[Long]{
      override def verify(v: String): Long = v.toLong
    }
  implicit val floatEv: ArgTypeEvidence[Float] =
    new ArgTypeEvidence[Float]{
      override def verify(v: String): Float = v.toFloat
    }
  implicit val doubleEv: ArgTypeEvidence[Double] =
    new ArgTypeEvidence[Double]{
      override def verify(v: String): Double = v.toDouble
    }

  implicit val pathEv: ArgTypeEvidence[Path] =
    new ArgTypeEvidence[Path]{
      override def verify(v: String): Path = Paths.get(v)
    }
  implicit val fileEv: ArgTypeEvidence[File] =
    new ArgTypeEvidence[File]{
      override def verify(v: String): File = new File(v)
    }
}
