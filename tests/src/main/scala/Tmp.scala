import java.nio.file.Path

import com.github.cuzfrog.scmd._


object Tmp {

  @ScmdDef
  class CatDef {
    appDef(name = "cat", shortDescription = "Concatenate files.", fullDescription = null)
    appDefCustom(
      "About" -> "this is a test app",
      "Organization" -> "com.github.cuzfrog",
      "null" -> null)

    //val badParam = paramDef[String](description = "throw exception")
    val cat = cmdDef(description = "Concatenate contents of files.")
    val files = paramDef[List[Path]](description = "Paths of files to concatenate.", isMandatory = true)
    val newLine = optDef[Boolean](description = "Add new line end to every file", abbr = "f")
  }

  @ScmdValid
  class CatValidation(argDef: CatDef) {
    validation(argDef.files) { files =>
      if (files.isEmpty) throw new AssertionError("List should not be empty, because it's mandatory.")
      files.foreach { f =>
        if (!f.toFile.exists()) throw new IllegalArgumentException(s"$f not exists.")
      }
    }
  }


  class CatRoute(argDef: CatDef) {

    import ScmdRouteDSL._

    val route =
      cmd(argDef.cat) {
        opt(argDef.newLine){ nl=>
          println(nl)
        }
      }

  }


  def main(args: Array[String]): Unit = {
    val parser = new CatDef
    println("-----------App info------------")
    println(parser.appInfoString)
    println("-----------Arg tree------------")
    println(parser.argTreeString)
    println("---------Parse result----------")

  }
}

