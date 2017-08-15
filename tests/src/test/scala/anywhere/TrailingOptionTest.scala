package anywhere

import java.nio.file.Path

import Scmd._
import com.github.cuzfrog.scmd.{ScalacheckIntegration, ScmdDefTest}
import org.junit._

class TrailingOptionTest {

  import scmdValueConverter._

  @Test
  def test1(): Unit = {
    assert(List("-a=5", "cmd1", "-b", "sthstr").parse.opta.value.contains(5))

    val parsed2 = List("cmd1", "sthstr", "-a=5", "-b").parse
    assert(parsed2.opta.value.contains(5))
    assert(!parsed2.optb.value)

    assert(List("-a=5", "cmd1", "-b", "sthstr", "/path/file").parse.opta.value.contains(5))

    val parsed4 = List("cmd1", "sthstr", "/path/file", "-a=5", "-b").parse
    assert(parsed4.opta.value.contains(5))
    assert(!parsed4.optb.value)
  }

  private implicit class ParseOps(in: List[String]) {
    def parse: TrailingOptionDefs = new TrailingOptionDefs(in).parse
  }

  @ScmdDefTest
  private class TrailingOptionDefs(args: Seq[String])
    extends ScmdDefStub[TrailingOptionDefs] {
    val opta = optDef[Int](abbr = "a")

    val cmd1 = cmdDef()
    val optb = optDef[Boolean](abbr = "b").withDefault(true)
    val paramb = paramDef[String]().mandatory
    val paramc = paramDef[Path]()

    val cmd2 = cmdDef()
  }
}
