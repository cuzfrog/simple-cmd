package com.github.cuzfrog.scmd.macros

import monocle.Traversal
import monocle.macros.syntax.lens._

import scala.collection.mutable
import scala.meta._
import scala.meta.contrib._
import scalaz.std.list._

/**
  * Help Class to provide implicit scope of app.
  *
  * In scmd, multiple apps could be defined within same project.
  * They are distinguished by their names.
  */
private case class AppContext(name: String)

/**
  * Db to restore and pass shared compile time information
  * between different scmd annotated classes.
  *
  * scmd annotated classes need to communicate with each other for argument validation etc.
  * Some need to be done at compile time.
  *
  * Because of compilation order, this design must be with caution.
  *
  * Thread-safe by synchronization to guard against possible concurrent compilation.
  */
private object AppRegister {
  private case class AppInternal(name: String,
                                 defClassType: Type,
                                 argDefs: List[ArgExtension] = List(),
                                 routes: Seq[TermRoute] = List())
  private case class ArgExtension(arg: RawArg,
                                  validFunc: Option[TermValidation] = None)

  private[this] val appRepository: mutable.Map[String, AppInternal] = mutable.Map.empty

  def registerApp(name: String, defClassType: Type): AppContext = {
    appRepository.get(name) match {
      case Some(app) =>
        abort(s"App:${app.name} defined in [${app.defClassType}] has already registered.")
      case None =>
        appRepository.put(name, AppInternal(name, defClassType))
        AppContext(name)
    }
  }

  /**App name is inferred by the class type name.*/
  def registerAppByType(className: Type): AppContext = {
    val appNamePossible = this.inferAppName(className)
    appRepository.put(appNamePossible, AppInternal(appNamePossible, className))
    AppContext(appNamePossible)
  }

  /** Pipe this arg, register and return it. */
  def registerArg(arg: RawArg)(implicit c: AppContext): RawArg = this.synchronized {
    val appInternal = appRepository.get(c.name) match {
      case Some(app) =>
        if (app.argDefs.exists(_.arg.arg.name == arg.arg.name)) {
          abort(arg.pos, s"Name conflict of arg:${arg.arg.name}")
        }
        app.lens(_.argDefs).modify(_ :+ ArgExtension(arg))
      case None => abort(arg.pos, s"App:${c.name} has not been registered.")
    }
    appRepository.put(c.name, appInternal)
    arg
  }

  /** Pipe this validation, register and return it. */
  def registerValidation(validation: TermValidation)
                        (implicit c: AppContext): TermValidation = this.synchronized {
    val appInternal = appRepository.get(c.name) match {
      case Some(app) =>
        if (!app.argDefs.exists(_.arg.arg.name == validation.argName)) {
          abort(validation.pos, s"Cannot find arg:${validation.argName}")
        }
        app.lens(_.argDefs).composeTraversal(eachArgExt).modify {
          case argExt if argExt.arg.arg.name == validation.argName => argExt.copy(validFunc = Some(validation))
          case other => other
        }
      case None => abort(validation.pos, s"No such app:${c.name}")
    }
    appRepository.put(c.name, appInternal)
    validation
  }

  def acquireAppByType(defClassType: Type): Option[AppContext] = {
    appRepository.find(_._2.defClassType isEqual defClassType) map {
      case (k, _) => AppContext(k)
    }
  }


  def inferAppByType(className: Type): Option[AppContext] = {
    val appNamePossible = this.inferAppName(className)
    appRepository.get(appNamePossible).map(_ => AppContext(appNamePossible))
  }

  // =============== helpers ================
  private val eachArgExt = Traversal.fromTraverse[List, ArgExtension]
  private val ClassNameExtractor = """(\w+)[A-Z]{1}\w*""".r
  private def inferAppName(className: Type): String = {
    className.syntax match {
      case ClassNameExtractor(n) => n
      case n => n
    }
  }
}