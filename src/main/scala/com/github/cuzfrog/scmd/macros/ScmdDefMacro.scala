package com.github.cuzfrog.scmd.macros


import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.macros.argutils.ArgUtils

import scala.collection.immutable
import scala.meta._

private class ScmdDefMacro extends ScmdMacro {


  /** Override this for testing. */
  protected val isTestMode: Boolean = false

  final def expand(mods: immutable.Seq[Mod],
                   name: Type.Name,
                   ctorMods: immutable.Seq[Mod],
                   paramss: immutable.Seq[immutable.Seq[Term.Param]],
                   stats: immutable.Seq[Stat]): Stat = {
    /** For testing. */
    val privateMod = if (isTestMode) mod"private[scmd]" else mod"private[this]"

    /** args passed in from constructor. */
    val argsParam = paramss.flatten.headOption match {
      case Some(param"..$mods $name: Seq[String]") => Term.Name(name.value)
      case _ => abort(s"$name's first parameter must be of type Seq[String] to accept arguments.")
    }


    val appInfo = TermAppInfo.collectAppInfo(stats)
    val appName: String = {
      val inferredName = if (name.value.endsWith("Def")) name.value.dropRight(3) else name.value
      appInfo.appInfo.name.getOrElse(inferredName.toLowerCase)
    }
    /**
      * A TermArg is macro time term of arg Node.
      *
      * This step collects arg defs from source code, checking syntax,
      * then turn them into Node terms.
      */
    val argDefs = TermArg.collectTermArg(stats, appName)

    /**
      * ArgTree represents structure of user defined args.
      * A TermArgTree is a premature ArgTree consisting of Terms, Which contains the topological information.
      *
      * This step build an TermArgTree, then turn it into a single macro-reify-ready Term.
      *
      * This step needs a ScmdRuntime instance to execute.
      * ScmdRuntime serves as a runtime agent to instantiate and encapsulate all needed scmd classes.
      */
    val argTreeBuild = {
      val globalMutualLimitations = TermTree.collectArgGlobalLimitations(stats)
      val treeBuilder = TreeBuilder.builder
      TermTree.collectTreeDefDsl(stats) match {
        case Nil =>
          /* by the order of user-defined args in source code */
          treeBuilder.buildArgTreeByIdx(appName, argDefs, globalMutualLimitations).defnTerm
        case dslParams =>
          /* by tree def dsl */
          treeBuilder.buildArgTreeByDSL(
            appName, argDefs, dslParams, globalMutualLimitations).defnTerm
      }
    }

    /** Method expose to validation class for runtime manipulation. */
    val public_def_addValidation =
      q"""def addValidation[T](argName:String,func: T => Unit): this.type = {
            scmdRuntime.addValidation(argName,func)
            this
          }"""

    /**
      * Return a new defClass after done parsing.
      *
      * Built-in priors are run in the new defClass.
      * This does not conflict with client-defined control flow.
      * Because when a prior arg is matched, args(cmds) afterward are ignored,
      * they are not met to execute client code.
      */
    val public_def_parsed = {
      val termParamss = paramss.map(_.map(param => Term.Name(param.name.value)))
      q"""def parsed: $name = {
            val evaluatedDefClass = try{
              scmdRuntime.parse($argsParam)
              scmdRuntime.runBuiltInPriors()
              new ${Ctor.Ref.Name(name.value)}(...$termParamss){
                ..${ArgUtils.convertParsed(stats)}
              }
            }catch{
              case e: ScmdException=> scmdRuntime.handleException(e)
            }
            //scmdRuntime.clean()
            evaluatedDefClass
          }"""
    }
    //todo: clean runtime after completion of parsing.

    val addMethods = List(
      q"""private[this] val scmdRuntime:ScmdRuntime = {
             val runtime = ScmdRuntime.create
             ${appInfo.term} //execute scmdRuntime to build an appInfo
             $argTreeBuild //execute scmdRuntime to build an argTree
             runtime
          }""",
      q"def appInfoString:String = scmdRuntime.appInfoString",
      q"def argTreeString:String = scmdRuntime.argTreeString",
      q"def parsedSeqString:String = scmdRuntime.parsedSeqString",
      public_def_addValidation,
      q"def withValidation[T](vali: $name => T): this.type = {vali(this); this}",
      q"def runWithRoute[T](route: $name => ArgRoute): Boolean = {route(this.parsed).run}",
      public_def_parsed,
      q"def parse():Unit = scmdRuntime.parse($argsParam)"
    )

    //abort("dev...")
    q"""..$mods class $name ..$ctorMods (...$paramss){
          import $TERM_pkg_scmd._
          import $TERM_pkg_scmd.runtime._
          import runtime.ScmdRuntime
          ..${stats.map(ArgUtils.addExplicitType)}
          ..$addMethods
        }"""
  }
}
