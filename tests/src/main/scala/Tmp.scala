import java.nio.file.Path

import com.github.cuzfrog.scmd._


object Tmp {

  @ScmdDef
  class CatArgDef {
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

  def main(args: Array[String]): Unit = {
    println((new CatArgDef).appInfoString)
    println((new CatArgDef).argTreeString)
  }
}

