import java.nio.file.Path

import com.github.cuzfrog.scmd._


object Tmp {

  def main(args: Array[String]): Unit = {
    CatArgDef.argDefs.foreach(println)
  }
}

@ScmdDef
object CatArgDef {
  //val badParam = paraDef[String](description = "throw exception")
  val cat = cmdDef(description = "Concatenate contents of files.")
  val files = paraDef[Seq[Path]](description = "Paths of files to concatenate.", isMandatory = true)
}