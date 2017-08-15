package anywhere

import Scmd._
import com.github.cuzfrog.scmd.internal.SimpleLogging
import com.github.cuzfrog.scmd.{ScalacheckIntegration, ScmdDefTest}
import com.github.cuzfrog.scmd.runtime.ArgParseException
import org.junit._
import org.scalacheck.Prop.forAll
import org.scalacheck.Test.Parameters

class OptionalityTest extends ScalacheckIntegration {
  override protected def scalacheckParams: Parameters =
    Parameters.defaultVerbose.withMaxSize(1)

  import scmdValueConverter._

  @Test
  def test1(): Unit = {
    val prop = forAll(arbInt, arbStr, arbStr, arbStr) { case (int, str1, str2, strM) =>
      val parsed = List(int, str1, str2, strM).map(_.toString).parse
      import parsed._
      debug(paramb.value + "|" + str1 + str2)

      //parama.value.contains(int)
      paramb.value == Seq(str1, str2) &&
        paramM.value == strM
    }
    assert(prop)
  }

  @Test
  def test2(): Unit = {
    val parsed = List("1","str1","str2","strM").parse
    import parsed._

    assert(parama.value.contains(1))
  }

  private implicit class ParseOps(in: List[String]) {
    def parse: OptionalityDefs = new OptionalityDefs(in).parse
  }

  @ScmdDefTest
  private class OptionalityDefs(args: Seq[String]) extends ScmdDefStub[OptionalityDefs] {
    val parama = paramDef[Int]()
    val paramb = paramDefVariable[String]()
    val paramc = paramDef[Int]() //should trigger compile-time error
    val paramM = paramDef[String]().mandatory
    val paramd = paramDef[String]()
  }
}
