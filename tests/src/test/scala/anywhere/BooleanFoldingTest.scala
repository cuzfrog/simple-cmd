package anywhere

import Scmd._
import com.github.cuzfrog.scmd.ScmdDefTest
import org.junit._

class BooleanFoldingTest {

  import BooleanFoldingTest._
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


  private implicit class ParseOps(in: List[String]) {
    def parse: BooleanFoldingDefs = new BooleanFoldingDefs(in).parse
  }
}

object BooleanFoldingTest {
  @ScmdDefTest
  private class BooleanFoldingDefs(args: Seq[String]) extends ScmdDefStub[BooleanFoldingDefs] {
    val opta = optDef[Boolean](abbr = "a")
    val optb = optDef[Boolean](abbr = "b")
    val optc = optDef[Boolean](abbr = "c")
    val optD = optDef[Boolean](abbr = "D")
    val optE = optDef[Boolean](abbr = "E")
    val optF = optDef[Boolean](abbr = "F")
    val optFG = optDef[Boolean](abbr = "FG")
    val optXX = optDef[Boolean](abbr = "XX")
    val optYy = optDef[Boolean](abbr = "Yy")
  }
}