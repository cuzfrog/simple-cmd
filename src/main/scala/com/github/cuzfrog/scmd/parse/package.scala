package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

/**
  * Created by cuz on 17-7-16.
  */
package object parse {
  private[parse] trait Parser[A, R] {
    def parse(a: A): R
  }

  private[parse] implicit class ParseOps[A: Parser[A, R], R](a: A) {
    private val parser = implicitly[Parser[A, R]]
    def parsed: R = parser.parse(a)
  }

  private[parse] type AnchorEither = Either[ArgParseException, Seq[ValueAnchor]]



  
  private[parse] implicit class HSeqOps[A](a:A){
    def collectWithType[T]:Seq[A]
  }
}
