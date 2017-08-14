package com.github.cuzfrog.scmd.macros


import com.github.cuzfrog.scmd.AppInfo
import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.macros.argutils.ArgUtils

import scala.meta._

private class ScmdDefMacro(isTestMode: Boolean = true) extends ScmdMacro {

  def expand(cls: Defn.Class): Stat = {
    val mods = cls.mods
    val name = cls.name
    val ctorMods = cls.ctor.mods
    val paramss = cls.ctor.paramss
    val stats = cls.templ.stats.getOrElse(Nil)

    /** For testing. */
    val privateMod = if (isTestMode) Nil else List(mod"private[this]")

    /** args passed in from constructor. */
    val argsParam = paramss.flatten.headOption match {
      case Some(param"..$mods $name: Seq[String]") => Term.Name(name.value)
      case _ => abort(s"$name's first parameter must be of type Seq[String] to accept arguments.")
    }


    val appInfo = {
      val inferredName = if (name.value.endsWith("Def")) name.value.dropRight(3) else name.value
      TermAppInfo.collectAppInfo(stats, inferredName.toLowerCase)
    }
    implicit val _appInfo: AppInfo = appInfo.appInfo
    /**
      * A TermArg is macro time term of arg Node.
      *
      * This step collects arg defs from source code, checking syntax,
      * then turn them into Node terms.
      */
    val rawArgs = ArgUtils.collectRawArg(stats)
    val argDefs = TermArg.builtInArgs ++ rawArgs.map(TermArg.toTermArg)

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
          treeBuilder.buildArgTreeByIdx(appInfo, argDefs, globalMutualLimitations).defnTerm
        case dslParams =>
          /* by tree def dsl */
          treeBuilder.buildArgTreeByDSL(
            appInfo, argDefs, dslParams, globalMutualLimitations).defnTerm
      }
    }

    /** Method expose to validation class for runtime manipulation. */
    val public_def_addValidation =
      q"""def addValidation[T](argName: scala.Symbol,func: T => Unit): this.type = {
            scmdRuntime.addValidation(argName,func)
            this
          }"""

    /** Parse args and return an evaluated def class. */
    val private_def_parsedWithoutRun = {
      val termParamss = paramss.map(_.map(param => Term.Name(param.name.value)))
      q"""private lazy val parsedWithoutRun: $name = {
            val evaluatedDefClass = try{
              scmdRuntime.parse($argsParam)
              new ${Ctor.Ref.Name(name.value)}(...$termParamss){
                ..${ArgUtils.convertParsed(rawArgs)}
              }
            }catch{
              case e: ScmdException=> scmdRuntime.handleException(e)
            }
            ..${if(isTestMode) Nil else List(q"scmdRuntime.clean()")}
            evaluatedDefClass
          }"""
    }

    /** For running built-in priors. This route could be merged with client defined route. */
    val private_def_builtInRoute = {
      q"""def builtInRoute(defCls: $name)
                          (implicit consoleType: ConsoleType): ArgRoute = {
             import ScmdRouteDSL._
             app.runOnPrior(defCls.help) {
               println(scmdRuntime.usageString)
             }.runOnPrior(defCls.version) {
               println(defCls.appInfo.version.getOrElse("No version number."))
             }.runThrough(println("built-in route run through"))
          }"""
    }

    /** Client api. Return a new defClass after done parsing and running built-in route. */
    val public_def_parsed = {
      q"""def parsed: $name = {
            val evaluatedDefClass = this.parsedWithoutRun
            import ArgRoute._
            this.builtInRoute(evaluatedDefClass).execute
            evaluatedDefClass
          }"""
    }

    /** Client api. Run with a client-provided route. Built-in route is prepended to client route. */
    val public_def_runWithRoute = {
      q"""def runWithRoute(route: $name => ArgRoute): Boolean = {
            val evaluatedDefClass = this.parsedWithoutRun
            import ArgRoute._
            //see ArgRoute for the doc of methods below.
            mergePriors(this.builtInRoute(evaluatedDefClass), route(evaluatedDefClass)).execute
          }"""
    }

    val addMethods = List(
      q"""..$privateMod val scmdRuntime:ScmdRuntime = {
             val runtime = ScmdRuntime.create
             $argTreeBuild //execute scmdRuntime to build an argTree/appInfo
             runtime
          }""",
      q"implicit val appInfo:AppInfo = scmdRuntime.getAppInfo",
      private_def_parsedWithoutRun,
      private_def_builtInRoute,
      public_def_addValidation,
      q"def withValidation[T](vali: $name => T): this.type = {vali(this); this}",
      public_def_runWithRoute,
      q"def defaultUsageString(implicit consoleType: ConsoleType): String = scmdRuntime.usageString",
      public_def_parsed
    )

    val testMethods = if (isTestMode) List(
      q"def appInfoString:String = scmdRuntime.appInfoString",
      q"def argTreeString:String = scmdRuntime.argTreeString",
      q"def parsedSeqString:String = scmdRuntime.parsedSeqString",
      q"def parse():Unit = scmdRuntime.parse($argsParam)"
    ) else Nil

    //abort("dev...")
    q"""..$mods class $name ..$ctorMods (...$paramss){
          import $TERM_pkg_scmd._
          private val defApiImport = new ScmdDefApi{}
          import defApiImport._
          import $TERM_pkg_scmd.runtime._
          import console.ConsoleType
          //import runtime.ScmdRuntime
          ..${ArgUtils.builtInPriorsStub}
          ..${ArgUtils.addExplicitType(rawArgs)}
          ..$addMethods
          ..$testMethods
        }"""
  }
}
