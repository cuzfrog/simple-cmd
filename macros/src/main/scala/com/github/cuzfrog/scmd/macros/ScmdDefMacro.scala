package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.macros.Constants._

import scala.collection.immutable
import scala.meta._

private[scmd] class ScmdDefMacro extends ScmdMacro {

  /** Override this for testing. */
  protected val isTestMode: Boolean = false

  final def expand(mods: immutable.Seq[Mod],
                   name: Type.Name,
                   ctorMods: immutable.Seq[Mod],
                   paramss: immutable.Seq[immutable.Seq[Term.Param]],
                   stats: immutable.Seq[Stat]): Stat = {
    /** For testing. */
    val privateMod = if (isTestMode) mod"private[scmd]" else mod"private[this]"

    /**
      * Annotated classes need to communicate with each other.
      * This is done through AppRegister.
      */
    val appInfo = TermAppInfo.collectAppInfo(stats)
    implicit val appContext: AppContext = appInfo.appInfo.name match {
      case Some(n) => AppRegister.registerApp(n, name)
      case None => AppRegister.registerAppByType(name)
    }

    val appInfoBuild = appInfo.term

    /**
      * A RawArg is macro time instance of arg definition.
      * A TermArg is macro time term of arg Node.
      *
      * This step collects arg defs from source code, checking syntax, register,
      * then turn them into Node terms.
      */
    val argDefs = RawArg.collectRawArg(stats).map(AppRegister.registerArg).map(TermArg.raw2termArg)

    /**
      * ArgTree represents structure of user defined args.
      * A TermArgTree is a premature ArgTree consisting of Terms, Which contains the topological information.
      *
      * This step build an TermArgTree by the order of user-defined args in source code,
      * then turn it into a single macro-reify-ready Term.
      *
      * This step needs a ScmdRuntime instance to execute.
      * ScmdRuntime serves as a runtime agent to instantiate and encapsulate all needed scmd classes.
      */
    val argTreeBuild = TreeBuilder.buildArgTreeByIdx(argDefs).defnTerm


    //println(argTreeBuild.syntax)

    val addMethods = List(
      q"$privateMod val scmdRuntime:ScmdRuntime = ScmdRuntime.create",
      q"$appInfoBuild", //execute scmdRuntime to build an appInfo
      q"def appInfoString:String = scmdRuntime.appInfoString",
      q"$argTreeBuild", //execute scmdRuntime to build an argTree
      q"def argTreeString:String = scmdRuntime.argTreeString",
      q"def parse(args: Array[String]) = { args.foreach(println) }"
    )

    //abort("dev...")
    q"""..$mods class $name ..$ctorMods (...$paramss){
          import $TERM_pkg_scmd.parse.ScmdRuntime
          ..$stats
          ..$addMethods
        }"""
  }
}
