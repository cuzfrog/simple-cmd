package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.internal.SimpleLogging


private object TestUtils {


}


trait ScalacheckIntegration extends SimpleLogging {
  override protected implicit def loggerAgent: SimpleLogging.LoggerAgent = getClass.getSimpleName

  implicit def checkProp(p: org.scalacheck.Prop): Boolean = {
    import org.scalacheck.Test.Parameters
    val params = Parameters.defaultVerbose.withMinSuccessfulTests(500)
    val test = org.scalacheck.Test.check(params, p)
    if (test.succeeded < 10)
      err(s"Test discard: ${test.discarded}.")
    test.passed
  }
}