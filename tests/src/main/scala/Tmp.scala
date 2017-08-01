import java.nio.file.{Path, Paths}

import Scmd._

object Tmp {

  @ScmdDef
  class CatDef(args: Seq[String]) {
    appDef(name = "cat", shortDescription = "Concatenate files.", fullDescription = null)
    appDefCustom(
      "About" -> "this is a test app",
      "Organization" -> "com.github.cuzfrog",
      "null" -> null)

    val sharedParam = paramDef[String](description = "this should be shared by cmds below.")
    val cat = cmdDef(description = "Concatenate contents of files.")
    val files = paramDefVariable[Path](description = "Paths of files to concatenate.", isMandatory = true)
    val newLine = optDef[Boolean](description = "Add new line end to every file", abbr = "f")

//    import scmdTreeDefDSL._
//
//    argTreeDef(
//      cat(
//        files & newLine & files,
//        newLine
//      )
//    )

  }

  @ScmdValid
  class CatValidation(argDef: CatDef) {
    validation(argDef.files) { files =>
      if (files.isEmpty) throw new AssertionError("List should not be empty, because it's mandatory.")
      files.foreach { f =>
        println(s"Print in validation func:$f")
        //if (!f.toFile.exists()) throw new IllegalArgumentException(s"$f not exists.")
      }
    }
  }


  class CatRoute(argDef: CatDef) {

    import scmdRouteDSL._

    val route =
      cmd(argDef.cat) {
        opt(argDef.newLine) { nl =>
          expectParam(argDef.files) { files =>
            println(nl)
            println(files)
          }
        }
      }

  }

  import scmdValueConverter._

  def main(args: Array[String]): Unit = {
    val conf = (new CatDef(args))
      .withValidation(new CatValidation(_))
    println("-----------App info------------")
    println(conf.appInfoString)
    println("-----------Arg tree------------")
    println(conf.argTreeString)
    val parsed: CatDef = conf.parsed
    println("---------Parsed node sequence:----------")
    println(conf.parsedSeqString)
    println("---------Parsed values:----------")
    val files = parsed.files.value
    println(files)
  }
}

