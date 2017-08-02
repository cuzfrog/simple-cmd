package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

package object runtime extends ArgTreeUtils {
  private[runtime] type AnchorEither = Either[ArgParseException, Seq[Anchor]]

  private[runtime] val NEWLINE: String = System.lineSeparator

  private[runtime] trait Parser[A, R] {
    def parse(a: A)(implicit c: Context): R
  }
  private[runtime] implicit class ParseOps[A, R](a: A)
                                                (implicit ev: Parser[A, R], context: Context) {
    private val parser = implicitly[Parser[A, R]]
    def parsed: R = parser.parse(a)
  }

  private[runtime] implicit class HSeqOps[N <: ValueNode[_]](a: Seq[N]) {
    def collectWithType[T <: NodeTag[T] : ClassTag]: Seq[T] = {
      val classTag = implicitly[ClassTag[T]]
      a.collect {
        case node if node.tpe == classTag => node.asInstanceOf[T]
      }
    }
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

  private[runtime] trait Convertible[A, R] {
    def convertTo(a: A): R
  }
  private[runtime] implicit class ConversionOps[A](a: A) {
    def convertTo[R](implicit ev: Convertible[A, R]): R = ev.convertTo(a)
  }

  private[runtime] sealed trait ConsoleType
  private[runtime] trait ManualEvidence[A] {
    def genFullManual(a: A)(implicit consoleType: ConsoleType): String
    def genSimpleManual(a: A)(implicit consoleType: ConsoleType): String
  }
  private[runtime] implicit class ManualGenerationOps[A: ManualEvidence](a: A) {
    private val ev: ManualEvidence[A] = implicitly[ManualEvidence[A]]
    private implicit val consoleType = new ConsoleType {}//todo: detect console type.
    def genFullManual: String = ev.genFullManual(a)
    def genSimpleManual: String = ev.genSimpleManual(a)
  }
}
