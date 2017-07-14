package com.github.cuzfrog.scmd.macros


import com.github.cuzfrog.scmd.{Command, Defaults, OptionArg, Parameter}

import scala.meta._
import scala.collection.immutable


private[scmd] object ScmdDefMacro {

  def expand(name: Term.Name, stats: immutable.Seq[Stat]): Defn.Object = {

    val argDefs = RawArg.collectRawArg(stats).map(TermArg.raw2termArg)

    val argGraph = GraphBuilder.buildArgGraphByIdx(argDefs).defnRuntimeTerm

    //println(argGraph.syntax)

    val importScmd = q"import com.github.cuzfrog.scmd._"
    val addMethods = Seq(
      q"val argGraph: com.github.cuzfrog.scmd.ArgGraph = $argGraph",
      q"def parse(args: Array[String]): Unit = { args.foreach(println) }"
    )
    val moreStats =(importScmd +: stats) ++ addMethods

    //abort("dev...")

    q"object $name { ..$moreStats }"
  }
}

