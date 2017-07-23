package com.github.cuzfrog.scmd.macros

import scala.collection.immutable
import scala.meta._


private[scmd] class ScmdValidMacro extends ScmdMacro {
  final def expand(mods: immutable.Seq[Mod],
                   name: Type.Name,
                   ctorMods: immutable.Seq[Mod],
                   paramss: immutable.Seq[immutable.Seq[Term.Param]],
                   stats: immutable.Seq[Stat]): Stat = {

    /*
     * Validation defining class needs to communicate with argument defining class.
     * This is done through AppRegister.
     *
     * Try to find current app by:
     *  1. parameters passed in.
     *  2. Inferring by name of the class.
     */
    val apps = paramss.flatMap(_.flatMap {
      case param"..$mods $name: $tpe = $_" =>
        val t = Type.Name(tpe.get.syntax)
        AppRegister.acquireAppByType(t)
    })
    if (apps.nonEmpty) abort("More than one app found by parameters.")
    implicit val appContext: AppContext = apps.headOption match {
      case Some(app) => app
      case None =>

        AppRegister.inferAppByType(name).getOrElse(abort(s"Cannot infer app by this class:$name"))
    }

    /**
      * Client defined validations against arguments.
      *
      * This step extracts validation from class stats and register them.
      */
    val validations = TermValidation.collectValidation(stats).map(AppRegister.registerValidation)

    q"""..$mods class $name ..$ctorMods (...$paramss){

        }"""
  }
}
