package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

/**
  * Created by cuz on 17-7-16.
  */
package object parse {
  private[parse] trait Parser[A, R] {
    def parse(a: A): R
  }

  private[parse] implicit class ParseOps[A, R](a: A)(implicit ev: Parser[A, R]) {
    private val parser = implicitly[Parser[A, R]]
    def parsed: R = parser.parse(a)
  }


  private[parse] implicit class HSeqOps[N <: ValueNode](a: Seq[N]) {
    def collectWithType[T <: NodeTag[T] : ClassTag]: Seq[T] = {
      val classTag = implicitly[ClassTag[T]]
      a.collect {
        case node if node.tpe == classTag => node.asInstanceOf[T]
      }
    }
  }

  private[parse] trait Countable[A] {
    def countMandatory(a: A): Int
  }
}
