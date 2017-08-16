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
    val prop = forAll(arbInt, arbStr, arbStr, arbStr, arbStr) { case (int, str1, str2, str3, strM) =>
      val parsed = List(int, str1, str2, str3, strM).map(_.toString).parse
      import parsed._
      //debug(paramb.value + "|" + str1 + str2)

      parama1.value.contains(int) &&
        parama2.value.contains(str1) &&
        paramb.value == Seq(str2, str3) &&
        paramM.value == strM
    }
    assert(prop)
  }

  @Test
  def test2(): Unit = {
    val parsed = List("-223", "str1", "-d", "str2", "strM", "-c").parse
    import parsed._
    assert(parama1.value.contains(-223))
    assert(parama2.value.contains("str1"))
    assert(paramb.value == Seq("str2"))
    assert(paramM.value == "strM")
  }

  @Test
  def test3(): Unit = {
    val parsed = List("-223", "-c", "-e", "str2", "-d", "strM").parse
    import parsed._
    assert(parama1.value.contains(-223))
    assert(parama2.value.contains("str2"))
    assert(paramb.value.isEmpty)
    assert(paramM.value == "strM")
  }

  @Test
  def test4(): Unit = {
    val parsed = List("-223", "-e", "strM", "-c", "-d").parse
    import parsed._
    assert(parama1.value.contains(-223))
    assert(parama2.value.isEmpty)
    assert(paramb.value.isEmpty)
    assert(paramM.value == "strM")
  }

  @Test
  def test5(): Unit = {
    val parsed = List("-c", "strM", "-d").parse
    import parsed._
    assert(parama1.value.isEmpty)
    assert(parama2.value.isEmpty)
    assert(paramb.value.isEmpty)
    assert(paramM.value == "strM")
  }

  private implicit class ParseOps(in: List[String]) {
    def parse: OptionalityDefs = new OptionalityDefs(in).parse
  }

  @ScmdDefTest
  private class OptionalityDefs(args: Seq[String]) extends ScmdDefStub[OptionalityDefs] {
    val parama1 = paramDef[Int]()
    val opt1 = optDef[Boolean](abbr = "b")
    val parama2 = paramDef[String]()
    val opt2 = optDef[Boolean](abbr = "c")
    val paramb = paramDefVariable[String]()
    val opt3 = optDef[Boolean](abbr = "d")
    //val paramc = paramDef[Int]() //should trigger compile-time error
    val paramM = paramDef[String]().mandatory
    //val paramd = paramDef[String]() //should trigger compile-time error
    val opt4 = optDef[Boolean](abbr = "e")
  }
}
