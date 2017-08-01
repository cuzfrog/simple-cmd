package simulation

import java.nio.file.Path

import Scmd._

private object Eft {
  @ScmdDef
  private class EftDef(args: Seq[String]) {
    //arg tree is inferred by order below:
    val port = optDef[Int](abbr = "p", description = "manually specify tcp port to use")
    val debug = optDef[Boolean](description = "debug mode")
    val printCode = optDef[Boolean](description = "print connction address as hex string")
    val remoteAddress = paramDef[String](description = "address of remote node for connection.")

    val push = cmdDef(
      description = "push a file to pull node | publish a file and wait for pulling from remote.")
    val srcFile = paramDef[Path](description = "file to send.", isMandatory = true)

    val pull = cmdDef(
      description = "pull a file from push node | establish pull service and wait for pushing from remote.")
    val destDir = paramDef[Path](description = "dest dir to save pulled file.")
  }

  @ScmdValid
  private class EftValidation(eftDef: EftDef) {
    validation(eftDef.srcFile) { f =>
      if (!f.toFile.exists) println(s"(Cp Simulation) Exception: Source file $f not exists.")
    }
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
