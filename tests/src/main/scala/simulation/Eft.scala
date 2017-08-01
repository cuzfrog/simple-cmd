package simulation

import java.io.File

import Scmd._

private object Eft {
  @ScmdDef
  private class EftDef(args: Seq[String]) {
    val SRC = paramDefVariable[File](isMandatory = true)
    val DEST = paramDef[File](isMandatory = true)
    val recursive = optDef[Boolean](abbr = "R")
  }

  @ScmdValid
  private class EftValidation(cpDef: EftDef) {

  }

  def main(args: Array[String]): Unit = {
    val conf = (new EftDef(args))
      .withValidation(new EftValidation(_))
    println("-----------App info------------")
    println(conf.appInfoString)
    println("-----------Arg tree------------")
    println(conf.argTreeString)
    println("---------Parsed node sequence:----------")
    val parsed: EftDef = conf.parsed
    println(conf.parsedSeqString)
    println("---------Parsed values:----------")
  }
}
