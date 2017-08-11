import java.nio.file.{Path, Paths}

import Scmd._
import com.github.cuzfrog.scmd.ArgRoute

object Tmp {

  @ScmdDef
  class CatDef(args: Seq[String]) extends ScmdDefStub[CatDef] {
    appDef(name = "cat", shortDescription = "Concatenate files.", fullDescription = null)
    appDefCustom(
      "About" -> "this is a test app",
      "Organization" -> "com.github.cuzfrog",
      "null" -> null)

    val sharedParam = paramDef[String](description = "this should be shared by cmds below.")
    val m1 = optDef[String](description = "test m1 descr").withDefault("default m")
    val cat1 = cmdDef(description = "Concatenate contents of files.")
    val files = paramDefVariable[Path](description = "Paths of files to concatenate.").mandatory
    val newLine = optDef[Boolean](description = "Add new line end to every file", abbr = "f")
    val num = optDefVariable[Long](abbr = "N", description = "test num N")

    val properties = propDef[Int](flag = "D", description = "this is a property arg.")
    val help1 = priorDef(alias = Seq("-help1", "--help1"))
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

    validation(argDef.properties) { props =>
      println(s"Print in validation, value:$props")
      props.find { case (k, _) => k == "key1" }.foreach {
        case (_, v) => if (v <= 3) throw new IllegalArgumentException("Value of key1 must be greater than 3")
      }
    }
  }

  def CatRoute(argDef: CatDef): ArgRoute = {

    import scmdRouteDSL._
    import argDef._
    import scmdValueConverter._

    app.runOnPrior(version) {
      println(s"this is version, overridden.$version")
    }.run {
      cat1.runOnPrior(help1) {
        println("PriorArg help1 triggered.")
        println(help1)
      }.onConditions(
        newLine.expectTrue,
        properties.expectByKey("key1")(_.forall(_ > 6))
      ).run {
        println(s"Numbers are: ${num.value.mkString(",")} (with new line (and key1's value >6) )")
      } ~
        app.run {
          println(s"Numbers are: ${num.value.mkString(",")}")
          println(s"Files are: ${files.value.mkString(",")}")
          //val mOpt:Option[String] = m1.valueOpt
          println(s"NewLine: " + newLine.value)
          val m = m1.value
          println(s"M1:" + m)
        }
    }
  }


  def main(args: Array[String]): Unit = {
    import scmdValueConverter._
    val conf = (new CatDef(args))
      .withValidation(new CatValidation(_))
    println("-----------App info------------")
    println(conf.appInfoString)
    println("-----------Arg tree------------")
    println(conf.argTreeString)
    val result = conf.runWithRoute(CatRoute)
    println(s"Run with route result: $result")
    //    val parsed: CatDef = conf.parsed
    //    println("---------Parsed node sequence:----------")
    //    println(conf.parsedSeqString)
    //    println("---------Parsed values:----------")
    //    println("Help prior: " + parsed.help)
    //    println("props key2: " + parsed.properties("key2"))
    //    println("--------- Usage: ----------")
    //    println(conf.defaultUsageString)
  }
}

