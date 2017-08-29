package anywhere

import Scmd._
import com.github.cuzfrog.scmd.internal.SimpleLogging
import com.github.cuzfrog.scmd.{ScalacheckIntegration, ScmdDefTest}
import com.github.cuzfrog.scmd.runtime.ArgParseException
import org.junit._
import org.scalacheck.Prop.forAll

class PropertyArgTest extends ScalacheckIntegration {

  import scmdValueConverter._

  @Test
  def test1(): Unit = {
    val prop = forAll(arbInt, arbStr) { (int, str) =>
      val parsed = List("-Nkey1=" + int, "-Dkey1=" + str).parse
      import parsed._
      prop1("key1").contains(str) &&
        prop2("key1").contains(int)
    }
    assert(prop)
  }

  @Test
  def test2(): Unit = {
    val prop = forAll(arbInt, arbStr) { (int, str) =>
      val parsed = List("-Nkey1:" + int, "-Dkey1:" + str).parse
      import parsed._
      prop1("key1").contains(str) &&
        prop2("key1").contains(int)
    }
    assert(prop)
  }

  @Test(expected = classOf[ArgParseException])
  def test3(): Unit = {
    val parsed = List("-Nkey1:").parse
  }

  private implicit class ParseOps(in: List[String]) {
    def parse: PropertyArgDefs = new PropertyArgDefs(in).parse
  }

  @ScmdDefTest
  class PropertyArgDefs(args: Seq[String]) extends ScmdDefStub[PropertyArgDefs] {
    val prop1 = propDef[String](flag = "D")
    val prop2 = propDef[Int](flag = "N")
  }
}
