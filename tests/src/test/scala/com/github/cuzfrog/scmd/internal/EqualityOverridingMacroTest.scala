package com.github.cuzfrog.scmd.internal

import org.junit._
import org.junit.Assert._

class EqualityOverridingMacroTest {

  @EqualityOverridingMacro
  private case class A(entity: String, value: String)


  @Test
  def test1(): Unit = {

    val a1 = A("same", "different-asdfhgs")
    val a2 = A("same", "different-gf3443")

    assertEquals(a1, a2)
  }
}
