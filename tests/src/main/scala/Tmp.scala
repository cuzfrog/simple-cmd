import java.nio.file.Path

import com.github.cuzfrog.scmd._


object Tmp {

  @ScmdDef
  class CatArgDef {
    //val badParam = paramDef[String](description = "throw exception")
    val cat = cmdDef(description = "Concatenate contents of files.")
    val files = paramDef[Seq[Path]](description = "Paths of files to concatenate.", isMandatory = true)
    val newLine = optDef[Boolean](description = "Add new line end to every file",abbr = "f")
  }

  def main(args: Array[String]): Unit = {
    (new CatArgDef).parse(args)
  }
}

