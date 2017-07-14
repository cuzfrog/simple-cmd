package com.github.cuzfrog.scmd.macros


import com.github.cuzfrog.scmd.{Command, Defaults, OptionArg, Parameter}

import scala.meta._
import scala.collection.immutable


private[scmd] object ScmdDefMacro {

  def expand(name: Term.Name, stats: immutable.Seq[Stat]): Defn.Object = {

    val argDefs = RawArg.collectRawArg(stats).map(TermArg.raw2termArg)


    argDefs.foreach(t => println(t.arg.structure))

    //argDefs.foreach(println)
    //    println(ArgBuilder.buildArgGraphByIdx(argDefs))

    val addMethods = Seq(
      //q"val argDefs = _root_.scala.collection.Seq(..$argDefs)",
      q"def parse(args: Array[String]): Unit = { args.foreach(println) }"
    )
    val moreStats = stats ++ addMethods

    abort("dev...")

    q"object $name { ..$moreStats }"
  }
}

