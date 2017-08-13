package com.github.cuzfrog.scmd.internal

import com.github.cuzfrog.scmd.ScalacheckIntegration
import org.junit._
import org.junit.Assert._
import org.scalacheck.Prop._


class EqualityOverridingMacroTest extends ScalacheckIntegration{

  @EqualityOverridingMacro
  private case class A(entity: String, value: Int)

  @Test
  def reflexive(): Unit = {
    val prop = forAll { (entity: String, value: Int) =>
      val a = A(entity, value)
      a == a && a != null
    }
    assertTrue(prop)
  }

  @Test
  def symmetric(): Unit = {
    val prop = forAll { (e1: String, e2: String, v1: Int, v2: Int) =>
      (e1 == e2) ==
        (A(e1, v1) == A(e2, v2) && A(e2, v2) == A(e1, v1))
    }
    assertTrue(prop)
  }

  @Test
  def transitive(): Unit = {
    val prop = forAll { (e1: String, e2: String, e3: String,
                         v1: Int, v2: Int, v3: Int) =>
      (e1 == e2 && e2 == e3) ==
        (A(e1, v1) == A(e2, v2) && A(e2, v2) == A(e3, v3) && A(e1, v1) == A(e3, v3))
    }
    assertTrue(prop)
  }
  //consistency is implied.


}
