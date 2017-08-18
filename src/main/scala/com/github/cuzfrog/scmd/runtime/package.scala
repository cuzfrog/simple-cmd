package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

package object runtime extends ArgTreeUtils {
  private[runtime] type AnchorEither = Either[ArgParseException, Seq[Anchor]]

  private[runtime] val NEWLINE: String = ScmdUtils.NEWLINE

  private[runtime] trait Parser[A, R] {
    def parse(a: A)(implicit c: Context): R
  }
  private[runtime] implicit class ParseOps[A, R](a: A)
                                                (implicit ev: Parser[A, R], context: Context) {
    private val parser = implicitly[Parser[A, R]]
    def parsed: R = parser.parse(a)
  }

  private[runtime] trait Countable[A] {
    def count(a: A): Int
  }
  private[runtime] implicit class CountOps[A: Countable](a: A) {
    private val countable = implicitly[Countable[A]]
    def count: Int = countable.count(a)
  }

  private[runtime] trait Collectible[A] {
    def collect[R: ClassTag](a: A): Seq[R]
  }
  private[runtime] implicit class CollectOps[A: Collectible](a: A) {
    private val collectible = implicitly[Collectible[A]]
    /** Given a type R, try to collect all reference to R in an A. */
    def collect[R: ClassTag]: Seq[R] = collectible.collect(a)
  }
}
