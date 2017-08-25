package anywhere

import java.nio.file.Path

import Scmd._
import com.github.cuzfrog.scmd.runtime.ArgValidationException
import com.github.cuzfrog.scmd.{ScalacheckIntegration, ScmdDefTest, ScmdDefTestStub}
import org.junit._

class MutualLimitationTest extends ScalacheckIntegration{

  import scmdValueConverter._

  @Test
  def test1(): Unit = {
    val parsed = List("324","-a=2","-c","str2","/path/file").parse
    import parsed._
    debug(opta.value)
    assert(param1.value.contains(324))
    assert(opta.value.contains(2))
  }

  @Test(expected = classOf[ArgValidationException])
  def adConflict(): Unit = {
    val parsed = List("324","-a=2","-d=123","str2").parse
    import parsed._
    debug(opta.value)
    assert(param1.value.contains(324))
    assert(opta.value.contains(2))
  }

  @Test(expected = classOf[ArgValidationException])
  def cDeficit(): Unit = {
    val parsed = List("324","-a=2","str2").parse
    import parsed._
    debug(opta.value)
    assert(param1.value.contains(324))
    assert(opta.value.contains(2))
  }

  @ScmdDefTest
  private class OptionValueFoldingDefs(args: Seq[String])
    extends ScmdDefTestStub[OptionValueFoldingDefs] {
    val opta = optDef[Int](abbr = "a")
    val optb = optDef[String](abbr = "B")
    val optc = optDef[Boolean](abbr = "c")
    val optd = optDef[Long](abbr = "d")

    val param1 = paramDef[Int]()
    val param2 = paramDef[String]()
    val param3 = paramDef[Path]()

    import scmdTreeDefDSL._

    argTreeDef(
      (opta & optc) | (optb & optd),
      //optc | optd, //should trigger compile-time duplication error
      //opta & optb, //should trigger compile-time logic error
      param1,
      param2,
      param3
    )
  }

  private implicit class ParseOps(in: List[String]) {
    val defClass = new OptionValueFoldingDefs(in)
    println(defClass.argTreeString)
    def parse: OptionValueFoldingDefs = defClass.parse
  }
}
