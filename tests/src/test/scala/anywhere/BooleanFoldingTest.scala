package anywhere

import Scmd._
import com.github.cuzfrog.scmd.ScmdDefTest
import com.github.cuzfrog.scmd.runtime.ArgParseException
import org.junit._

class BooleanFoldingTest {

  import scmdValueImplicitConversion._

  @Test
  def test1(): Unit = {
    val parsed = List("-baEDc").parse
    assert(parsed.opta)
    assert(parsed.optb)
    assert(parsed.optc)
    assert(parsed.optD)
    assert(parsed.optE)
    assert(!parsed.optF)
  }

  @Test(expected = classOf[ArgParseException])
  def test2(): Unit = {
    val parsed = List("-baEDcFG").parse
  }

  private implicit class ParseOps(in: List[String]) {
    def parse: BooleanFoldingDefs = new BooleanFoldingDefs(in).parse
  }

  @ScmdDefTest
  private class BooleanFoldingDefs(args: Seq[String]) extends ScmdDefStub[BooleanFoldingDefs] {
    val opta = optDef[Boolean](abbr = "a")
    val optb = optDef[Boolean](abbr = "b")
    val optc = optDef[Boolean](abbr = "c")
    val optD = optDef[Boolean](abbr = "D")
    val optE = optDef[Boolean](abbr = "E")
    val optF = optDef[Boolean](abbr = "F")
    val optFG = optDef[Boolean](abbr = "FG")
  }
}