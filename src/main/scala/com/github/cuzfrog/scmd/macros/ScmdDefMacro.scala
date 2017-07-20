package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.RuntimeClassDefs

import scala.meta._
import scala.collection.immutable
import Constants._

private[scmd] class ScmdDefMacro extends ScmdMacro {

  /** Override this for testing. */
  protected val isTestMode: Boolean = false

  final def expand(name: Type.Name, stats: immutable.Seq[Stat], classDefs: RuntimeClassDefs.type): Stat = {

    val appInfo = TermAppInfo.collectAppInfo(stats)

    /**
      * A RawArg is macro time instance of arg definition.
      * A TermArg is macro time term of arg Node.
      *
      * This step collects arg defs from source code, checking syntax,
      * then turn them into Node terms.
      */
    val argDefs = RawArg.collectRawArg(stats).map(TermArg.raw2termArg)

    /**
      * ArgTree represents structure of user defined args.
      * A TermArgTree is a premature ArgTree consisting of Terms, Which contains the topological information.
      *
      * This step build an TermArgTree by the order of user-defined args in source code,
      * then turn it into a single macro-reify-ready Term.
      */
    val argTreeBuild = TreeBuilder.buildArgTreeByIdx(argDefs).defnTerm

    /** For testing. */
    val privateMod = if (isTestMode) mod"private[scmd]" else mod"private[this]"


    println(argTreeBuild.syntax)

    val addMethods = List(
      q"$privateMod val scmdRuntime:ScmdRuntime = ScmdRuntime.create",
      q"$privateMod def buildTree:ScmdRuntime = $argTreeBuild",
      q"def argTreeString:String = buildTree.argTreeString",
      q"def parse(args: Array[String]) = { args.foreach(println) }"
    )

    //abort("dev...")
    q"""class $name{
          import $TERM_pkg_scmd.parse.ScmdRuntime
          ..$addMethods
        }"""
  }
}


////    require(Option(appName).nonEmpty, "appName cannot be null.") //guards
////    require(appName != "", "appName cannot be empty. You could leave as default, which is: App")
