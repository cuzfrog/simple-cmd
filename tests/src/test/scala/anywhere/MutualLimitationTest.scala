package anywhere

import java.nio.file.Path

import Scmd._
import com.github.cuzfrog.scmd.{ScalacheckIntegration, ScmdDefTest}
import org.junit._

class MutualLimitationTest {

  @Test
  def test1(): Unit = {

  }

  @ScmdDefTest
  private class OptionValueFoldingDefs(args: Seq[String])
    extends ScmdDefStub[OptionValueFoldingDefs] {
    val opta = optDef[Int](abbr = "a")
    val optb = optDef[String](abbr = "B")
    val optc = optDef[Boolean](abbr = "c")
    val optd = optDef[Long](abbr = "d")

    val param1 = paramDef[Int]()
    val param2 = paramDef[String]()
    val param3 = paramDef[Path]()

    import scmdTreeDefDSL._

    argTreeDef(
      (param1 & opta) | (param2 & optb),
//      param1 & opta,
//      param2 & optb,
      // param1 | param2 inferred.
      param3 & optc & optd,
      //opta & optb, //should trigger compile-time error
      //param1 & param2 //should trigger compile-time error
    )
  }
}
