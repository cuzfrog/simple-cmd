package com.github.cuzfrog.scmd.macros

import scala.collection.mutable

import scala.meta._
import monocle.macros.syntax.lens._
import monocle.Traversal
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
                                 argDefs: List[ArgExtention] = List(),
                                 routes: Seq[TermRoute] = Seq())
  private case class ArgExtention(arg: RawArg,
                                  validFunc: Option[TermValidation] = None)

  private[this] val appRepository: mutable.Map[String, AppInternal] = mutable.Map.empty

  def registerArg(arg: RawArg)(implicit c: AppContext): RawArg = this.synchronized {
    val appInternal = appRepository.get(c.name) match {
      case Some(app) =>
        if (app.argDefs.exists(_.arg.arg.name == arg.arg.name)) {
          abort(arg.pos, s"Name conflict of arg:${arg.arg.name}")
        }
        app.lens(_.argDefs).modify(_ :+ ArgExtention(arg))
      case None => AppInternal(c.name, List(ArgExtention(arg)))
    }
    appRepository.put(c.name, appInternal)
    arg
  }

  def registerValidation(argName: String,
                         validFunc: TermValidation)
                        (implicit c: AppContext): TermValidation = this.synchronized {
    val appInternal = appRepository.get(c.name) match {
      case Some(app) =>
        if (!app.argDefs.exists(_.arg.arg.name == argName)) {
          abort(validFunc.pos, s"Cannot find arg:$argName")
        }
        app.lens(_.argDefs).composeTraversal(eachArgExt).modify {
          case argExt if argExt.arg.arg.name == argName => argExt.copy(validFunc = Some(validFunc))
          case other => other
        }
      case None => abort(validFunc.pos, s"No such app:${c.name}")
    }
    appRepository.put(c.name, appInternal)
    validFunc
  }

  // =============== helpers ================
  private val eachArgExt = Traversal.fromTraverse[List, ArgExtention]
}