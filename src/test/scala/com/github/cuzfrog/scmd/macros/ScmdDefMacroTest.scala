package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._

/**
  * Created by cuz on 7/19/17.
  */
private[scmd] object ScmdDefMacroTest {
  def expand(name: Type.Name, stats: immutable.Seq[Stat]): Defn.Class = {

    val argDefs = RawArg.collectRawArg(stats).map(TermArg.raw2termArg)

    val argTree = TreeBuilder.buildArgTreeByIdx(argDefs).defnTerm

    //println(argTree.syntax)

    val headers = List(
      q"import com.github.cuzfrog.scmd._"
    )
    val addMethods = List(
      q"def argTree: com.github.cuzfrog.scmd.parse.ArgTree = $argTree",
      q"""def parse(args: Array[String]) = { args.foreach(println) }"""
    )
    val moreStats = headers ++ stats ++ addMethods

    //abort("dev...")

    q"""
          class $name { ..$moreStats }
        """
  }
}
