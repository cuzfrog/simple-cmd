package simulation

import java.io.File

import Scmd._

object Cp {
  @ScmdDef
  private class CpDef(args: Seq[String]) {
    val SRC = paramDefVariable[File](isMandatory = true)
    val DEST = paramDef[File](isMandatory = true)
    val recursive = optDef[Boolean](abbr = "R")
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
    val parsed: CpDef = conf.parsed
    println(conf.parsedSeqString)
    println("---------Parsed values:----------")
  }
}
