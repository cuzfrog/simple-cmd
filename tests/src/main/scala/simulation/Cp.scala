package simulation

import java.io.File

import Scmd._

object Cp {
  @ScmdDef
  class CpDef(args: Seq[String]) extends ScmdDefStub{
    appDef(name = "cp",shortDescription = "copy file or dir.")
    val SRC = paramDefVariable[File]().mandatory
    val DEST = paramDef[File]().mandatory
    val recursive = optDef[Boolean](abbr = "R", description = "recursively copy the whole dir.")
  }

  @ScmdValid
  private class CpValidation(cpDef: CpDef) {
    validation(cpDef.SRC) { files =>
      files.foreach(f => if (!f.exists) println(s"(Cp Simulation) Exception: $f not exists."))
    }
  }

  def main(args: Array[String]): Unit = {
    val conf = (new CpDef(args))
      .withValidation(new CpValidation(_))
    println("-----------App info------------")
    println(conf.appInfoString)
    println("-----------Arg tree------------")
    println(conf.argTreeString)
    println("---------Parsed node sequence:----------")
    val parsed: CpDef = conf.parse
    println(conf.parsedSeqString)
    println("---------Parsed values:----------")
  }
}
