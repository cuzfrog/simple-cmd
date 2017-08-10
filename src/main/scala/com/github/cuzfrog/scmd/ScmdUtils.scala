package com.github.cuzfrog.scmd

private object ScmdUtils {
  trait Convertible[A, R] {
    def convertTo(a: A): R
  }
  implicit class ConversionOps[A](a: A) {
    def convertTo[R](implicit ev: Convertible[A, R]): R = ev.convertTo(a)
  }

  trait CanFormPrettyString[A] {
    def mkPrettyString(a: A): String
  }
  implicit class PrettyStringBuildOps[A: CanFormPrettyString](a: A) {
    def prettyString: String = implicitly[CanFormPrettyString[A]].mkPrettyString(a)
  }

  trait CanMerge[A, S] {
    def merge(a: A, stuff: S): A
  }
  def merge[A, B](a: A, stuff: B)
                 (implicit ev: CanMerge[A, B]): A = ev.merge(a, stuff)
  implicit class MergeOps[A, S](in: A)(implicit ev: CanMerge[A, S]) {
    def mergeWith(stuff: S): A = ev.merge(in, stuff)
  } //nested type cannot be inferred.

  trait CanMix[A, B] {
    def mix(a: A, _trait: B): A with B
  }
  def mix[A, B](a: A, _trait: B)
               (implicit ev: CanMix[A, B]): A with B = ev.mix(a, _trait)
  implicit class MixOps[A, B](in: A)(implicit ev: CanMix[A, B]) {
    def mixWith(_trait: B): A = ev.mix(in, _trait)
  }

  /**
    * A placeholder to make parameters optional.
    * This, is a method, can only be used as call-by-name parameter.
    *
    * For client's simplicity, `Option` is not used.
    */
  def Empty[T]: T =
    throw new IllegalArgumentException("Empty default value called. See scmd.Empty.")

  def throwIfEmptyDefault(arg: Argument[_]): Nothing =
    throw new IllegalArgumentException(s"Default value empty for ${arg.originalName}")
  def throwIfEmptyMandatory(arg: Argument[_]): Nothing =
    throw new IllegalArgumentException(s"Mandatory value empty for ${arg.originalName}")
}
