package com.github.cuzfrog.scmd.internal

import scala.language.experimental.macros

import scala.reflect.ClassTag
import scala.reflect.macros.whitebox


private[scmd] object RawArgMacro {

  import scala.meta.{Lit, Position, Term, abort}

  /** Extract parameter by enclosing val name. */
  def extract[T: ClassTag](params: Seq[Term.Arg])
                          (implicit pos: Position): Option[T] = macro RawArgMacroImpl.impl[T]

  /** This method should not be called directly. */
  def extractWithName[T: ClassTag](params: Seq[Term.Arg],
                                           paramName: String, pos: Position): Option[T] = {
    val value = params.map {
      case named: Term.Arg.Named => named
      case unnamed =>
        abort(pos, s"Definition must use named parameter in def for content:${unnamed.syntax}")
    }.collect { case Term.Arg.Named(Term.Name(n), v) if n == paramName => v }.headOption
    val tpe = implicitly[ClassTag[T]]

    val actual = tpe.runtimeClass match {
      case rc if rc == classOf[String] => value collect { case Lit.String(v) => v }
      case rc if rc == classOf[Boolean] => value collect { case Lit.Boolean(v) => v }
      case rc if rc == classOf[Term.Arg] => value
      case rc => throw new AssertionError(s"Type not coded for argDef def: $rc")
    }
    actual.map(_.asInstanceOf[T])
  }
}

private object RawArgMacroImpl {
  def impl[T: c.WeakTypeTag](c: whitebox.Context)(params: c.Tree)(classTag: c.Tree, pos: c.Tree): c.Tree = {
    import c.universe._
    val termName = c.internal.enclosingOwner.asTerm.name.toString

    val tpe = c.weakTypeOf[T]
    val objectName = q"com.github.cuzfrog.scmd.internal.RawArgMacro"
    val tree = q"$objectName.extractWithName[$tpe]($params,$termName,$pos)"
    //println(showCode(tree))
    tree
  }
}