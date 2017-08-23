package anywhere

import Scmd._
import com.github.cuzfrog.scmd.{ScalacheckIntegration, ScmdDefTest}
import org.junit._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Test, _}

class OptionValueFoldingTest extends ScalacheckIntegration {
  override protected def scalacheckParams: Test.Parameters =
    Test.Parameters.default.withMinSuccessfulTests(10)

  import scmdValueConverter._

  @Test
  def foldingTest(): Unit = {
    val prop = forAll(arbInt, arbStr) { case (int, str) =>
      val parsed = List("-a" + int, "-B" + str).parse
      parsed.opta.value.contains(int) &&
        parsed.optb.value.contains(str)

    }
    assert(prop)
  }

  @Test
  def boolFoldingTest(): Unit = {
    assert(List("-c=true").parse.optc.value)
    assert(List("-c=tRue").parse.optc.value)
    assert(!List("-c=faLSE").parse.optc.value)
    assert(List("--optc=True").parse.optc.value)
    assert(!List("--optc=faLSE").parse.optc.value)
  }

  @Test
  def evaluationTest(): Unit = {
    val prop = forAll(arbInt, arbStr) { case (int, str) =>
      val parsed = List("-a=" + int, "-B=" + str).parse
      parsed.opta.value.contains(int) &&
        parsed.optb.value.contains(str)
    }
    assert(prop)
  }

  @Test
  def evaluationLongTest(): Unit = {
    val prop = forAll(arbInt, arbStr) { case (int, str) =>
      val parsed = List("--opta=" + int, "--optb=" + str).parse
      parsed.opta.value.contains(int) &&
        parsed.optb.value.contains(str)
    }
    assert(prop)
  }

  @Test
  def spaceTest(): Unit = {
    val prop = forAll(arbInt, arbStr) { case (int, str) =>
      val parsed = List("-a", int.toString, "-B", str).parse
      parsed.opta.value.contains(int) &&
        parsed.optb.value.contains(str)
    }
    assert(prop)
  }

  @Test
  def spaceLongTest(): Unit = {
    val prop = forAll(arbInt, arbStr) { case (int, str) =>
      val parsed = List("--opta", int.toString, "--optb", str).parse
      parsed.opta.value.contains(int) &&
        parsed.optb.value.contains(str)
    }
    assert(prop)
  }

  private implicit class ParseOps(in: List[String]) {
    def parse: OptionValueFoldingDefs = new OptionValueFoldingDefs(in).parse
  }

  @ScmdDefTest
  private class OptionValueFoldingDefs(args: Seq[String])
    extends ScmdDefStub[OptionValueFoldingDefs] {
    val opta = optDef[Int](abbr = "a")
    val optb = optDef[String](abbr = "B")
    val optc = optDef[Boolean](abbr = "c")
  }
}
