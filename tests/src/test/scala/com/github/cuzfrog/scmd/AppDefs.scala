package com.github.cuzfrog.scmd

import java.nio.file.Path

/**
  * Created by cuz on 7/19/17.
  */
object AppDefs {
  @ScmdDefTest
  class CpDef {
    val src =
      paramDef[Seq[Path]](description = "Paths of files/dirs to copy.", isMandatory = true)
    val dst =
      paramDef[Path](description = "Path (of dir) to copy to.", isMandatory = true)
    val recursive = optDef[Boolean](description = "Copy dirs recursively", abbr = "R")
  }

  //@ScmdDefTest
  class EftDef {
    val port = optDef[Int](description = "Speicify listening port for contact.", abbr = "p")

    val push = cmdDef(description = "Pull a published file from push node.")


  }
}
