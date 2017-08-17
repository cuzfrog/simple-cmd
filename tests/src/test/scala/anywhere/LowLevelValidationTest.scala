package anywhere

import java.io.File

import Scmd._
import com.github.cuzfrog.scmd.runtime.{ArgValidationException, ScmdException, ScmdExceptionHandler}
import com.github.cuzfrog.scmd.{ScalacheckIntegration, ScmdDefTest, ScmdDefTestStub}
import org.junit._
import org.scalacheck.Prop._

import scala.util.Try

class LowLevelValidationTest extends ScalacheckIntegration {

  import scmdValueImplicitConversion._

  @Test
  def test1(): Unit = {
    val prop = forAll(arbInt) { int =>
      (int <= 9999) ==> {
        List(int.toString).parse.smallInt.contains(int)
      }
    }
    assert(prop)

    val prop2 = forAll(arbInt) { int =>
      (int > 9999) ==> {
        Try(List(int.toString).parse.smallInt).isFailure
      }
    }
    assert(prop2)
  }

  @ScmdValid
  class CatValidation(argDef: LowLevelValidationDefs) {

    import argDef._

    validation(smallInt) { value =>
      if (value > 9999) throw new IllegalArgumentException("too big")
    }
    validation(file) { f =>
      if (!f.exists()) throw new IllegalArgumentException("file does not exist")
    }
  }

  @ScmdDefTest
  private class LowLevelValidationDefs(args: Seq[String])
    extends ScmdDefTestStub[LowLevelValidationDefs] {
    val smallInt = paramDef[Int]()
    val file = optDef[File]()

    private implicit val dummyExceptionHandler: ScmdExceptionHandler[ScmdException] =
      (e: ScmdException) => throw e
  }



  private implicit class ParseOps(in: List[String]) {
    def parse: LowLevelValidationDefs =
      new LowLevelValidationDefs(in).withValidation(new CatValidation(_)).parse
  }
}
