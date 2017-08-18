package anywhere

import Scmd._
import com.github.cuzfrog.scmd.{ScalacheckIntegration, ScmdDefTest, ScmdDefTestStub}
import com.github.cuzfrog.scmd.runtime.ArgParseException
import org.junit._
import org.scalacheck.Prop._
import scala.collection.mutable.ArrayBuffer

class RoutingAndArgTreeTest extends ScalacheckIntegration {

  private val record: ArrayBuffer[String] = ArrayBuffer.empty

  @Before
  def clear(): Unit = {
    record.clear()
  }

  @Test
  def test1(): Unit = record.synchronized {
    val prop = forAll(arbInt, arbStr) { (int, str) =>
      (int > 1000) ==> {
        List("cmd1", str, "cmd2", "-a", int.toString).run &&
          record.contains(str) &&
          record.contains("moreThan1000")
      }
    }
    assert(prop)
  }

  @Test
  def test2(): Unit = record.synchronized {
    val prop = forAll(arbInt, arbStr) { (int, str) =>
      (int <= 1000) ==> {
        List("cmd1", str, "cmd2", "-a", int.toString).run &&
          record.contains(str) &&
          record.contains("within1000")
      }
    }
    assert(prop)
  }

  @Test
  def test3(): Unit = record.synchronized {
    val prop = forAll(arbStr) { (str) =>
      List("cmd1", str, "cmd2").run &&
        record.contains(str)
    }
    assert(prop)
  }

  @Test
  def test4(): Unit = record.synchronized {
    val prop = forAll(arbStr) { (str) =>
      List("cmd1", str, "cmd2", "-b").run &&
        record.contains(str) && record.contains("optb")
    }
    assert(prop)
  }

  @Test
  def test5(): Unit = record.synchronized {

    List("cmd1", "str", "cmd2", "-b").run

    assert(record.contains("str"))
    assert(record.contains("optb"))
    assert(record.length == 2)
  }

  @ScmdDefTest
  private class ArgDefs(args: Seq[String])
    extends ScmdDefTestStub[ArgDefs] {
    val cmd1 = cmdDef()
    val cmd2 = cmdDef()

    val opta = optDef[Int](abbr = "a")
    val optb = optDef[Boolean](abbr = "b")
    val param1 = paramDef[String]()
    val param2 = paramDef[String]()

    import scmdTreeDefDSL._

    argTreeDef(
      cmd1(
        param1,
        cmd2(
          opta | optb,
          param2
        )
      )
    )
  }

  private def route(argDefs: ArgDefs): ArgRoute = {
    import scmdRouteDSL._
    import argDefs._
    import scmdValueConverter._

    app.run {
      //println(s"app run|$record")
      cmd1.run {
        param1.value.foreach(record += _)
        //println("eval param1|" + record)
      } &
        cmd1.run {
          //println(s"cmd1 run|$record")
          cmd2.onConditions(
            opta.expect(_.exists(_ > 1000))
          ).run {
            record += "moreThan1000"
            //println(s"cmd2 run|$record")
          } ~
            cmd2.onConditions(
              opta.expect(_.exists(_ <= 1000))
            ).run {
              record += "within1000"
              //println(s"cmd2 run|$record")
            } ~
            cmd2.onConditions(
              optb.expectTrue
            ).run {
              record += "optb"
              //println(s"cmd2 run|$record")
            }
        }
    }
  }

  private implicit class ParseOps(in: List[String]) {
    def run: Boolean = new ArgDefs(in).runWithRoute(route)
  }
}
