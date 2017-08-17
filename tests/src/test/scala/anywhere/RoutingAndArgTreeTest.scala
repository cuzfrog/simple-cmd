package anywhere

import Scmd._
import com.github.cuzfrog.scmd.{ScmdDefTest, ScmdDefTestStub}
import com.github.cuzfrog.scmd.runtime.ArgParseException
import org.junit._

class RoutingAndArgTreeTest {

  @Test
  def test1(): Unit = {

  }


  @ScmdDefTest
  private class ArgDefs(args: Seq[String])
    extends ScmdDefTestStub[ArgDefs] {
    val cmd1 = cmdDef()
    val cmd2 = cmdDef()
    val cmd3 = cmdDef()

    val opta = optDef[Int](abbr = "a")
    val optb = optDef[Boolean](abbr = "b")
    val param1 = paramDef[Int]()
    val param2 = paramDef[String]()

    import scmdTreeDefDSL._

    argTreeDef(
      cmd1(
        param1,
        cmd2(
          opta | optb,
          cmd3(param2)
        )
      )
    )
  }

  private def route(argDefs: ArgDefs): ArgRoute = {
    import scmdRouteDSL._
    import argDefs._

    app.run(
      cmd1.run(

      )
    )
  }

  private implicit class ParseOps(in: List[String]) {
    def run: Boolean = new ArgDefs(in).runWithRoute(route)
  }
}
