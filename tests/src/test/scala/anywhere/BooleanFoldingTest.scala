package anywhere

import Scmd._
import com.github.cuzfrog.scmd.ScmdDefTest
import org.junit._

class BooleanFoldingTest {

  import scmdValueConverter._

  @Test
  def test1(): Unit = {
    //val parsed = List("-a").parse

   //println(parsed.opta.value)
  }


  @ScmdDefTest
  private class BooleanFoldingDefs(args: Seq[String]) extends ScmdDefStub[BooleanFoldingDefs] {
    val opta = optDef[Boolean](abbr = "a")
    val optb = optDef[Boolean](abbr = "b")
    val optc = optDef[Boolean](abbr = "c")
    val optD = optDef[Boolean](abbr = "D")
    val optE = optDef[Boolean](abbr = "E")
    val optFG = optDef[Boolean](abbr = "FG")
    val optXX = optDef[Boolean](abbr = "XX")
    val optYy = optDef[Boolean](abbr = "Yy")
  }

  private implicit class ParseOps(in: List[String]) {
    def parse: BooleanFoldingDefs = new BooleanFoldingDefs(in).parse
  }
}