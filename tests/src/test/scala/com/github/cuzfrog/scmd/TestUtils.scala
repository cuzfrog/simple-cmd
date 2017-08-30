package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.internal.SimpleLogging
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Test.Parameters


private object TestUtils {


}


trait ScalacheckIntegration extends SimpleLogging {
  override protected implicit def loggerAgent: SimpleLogging.LoggerAgent = getClass.getSimpleName
  protected def scalacheckParams: Parameters = Parameters.defaultVerbose

  protected def arbInt: Gen[Int] = Arbitrary.arbInt.arbitrary
  /**Non-empty arbitrary string.*/
  protected def arbStr: Gen[String] =
    Gen.alphaNumStr suchThat (s=>s.nonEmpty && !s.contains("\u0085"))

  implicit def checkProp(p: org.scalacheck.Prop): Boolean = {
    import org.scalacheck.Test.Parameters
    val test = org.scalacheck.Test.check(scalacheckParams, p)
    if (test.succeeded < 10)
      err(s"Test discard: ${test.discarded}.")
    test.passed
  }
}