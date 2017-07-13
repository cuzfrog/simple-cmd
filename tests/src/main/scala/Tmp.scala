import java.nio.file.Path

import com.github.cuzfrog.scmd._


object Tmp {

  def main(args: Array[String]): Unit = {
    CatArgDef.parse(args)
  }
}

@ScmdDef
object CatArgDef {
  val cat = cmdDef(description = "Concatenate contents of files.")
  val files = paraDef[Seq[Path]](description = "Paths of files to concatenate.", isMandatory = true)
}