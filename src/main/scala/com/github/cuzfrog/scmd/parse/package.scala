package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

package object parse extends ArgTreeUtils{
  type AnchorEither = Either[ArgParseException, Seq[Anchor[_]]]

  private[parse] trait Parser[A, R] {
    def parse(a: A)(implicit c: Context): R
  }

  private[parse] implicit class ParseOps[A, R](a: A)
                                              (implicit ev: Parser[A, R], context: Context) {
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
    def count(a: A): Int
  }

  private[parse] implicit class CountOps[A: Countable](a: A) {
    private val countable = implicitly[Countable[A]]
    def count: Int = countable.count(a)
  }

  private[parse] trait Collectible[A] {
    def collect[R: ClassTag](a: A): Seq[R]
  }

  private[parse] implicit class CollectOps[A: Collectible](a: A) {
    private val collectible = implicitly[Collectible[A]]
    /** Given a type R, try to collect all reference to R in an A. */
    def collect[R: ClassTag]: Seq[R] = collectible.collect(a)
  }
}
