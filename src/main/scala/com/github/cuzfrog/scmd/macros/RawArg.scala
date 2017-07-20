package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.{Argument, Command, Defaults, OptionArg, Parameter}

import scala.collection.immutable
import scala.meta._
import scala.reflect.ClassTag


private final case class RawArg(arg: Argument[_], idx: Int, tpe: Type)

private object RawArg {
  def collectRawArg(stats: immutable.Seq[Stat]): immutable.Seq[RawArg] = {
    stats.zipWithIndex collect {
      case (q"val $cmd: $_ = cmdDef(..$params)", idx) =>
        val description = extract[String](params, TERM_DESCRIPTION)
        RawArg(Command(cmd.syntax, description), idx, TYPE_NOTHING)

      case (q"val $para: $_ = paramDef[$tpe](..$params)", idx) =>
        val description = extract[String](params, TERM_DESCRIPTION)
        val isMandatory =
          extract[Boolean](params, TERM_IS_MANDATORY).getOrElse(Defaults.isMandatory)
        RawArg(
          Parameter(name = para.syntax, description = description, isMandatory = isMandatory)
          , idx, tpe
        )

      case (q"val $opt: $_ = optDef[$tpe](..$params)", idx) =>
        val description = extract[String](params, TERM_DESCRIPTION)
        val isMandatory =
          extract[Boolean](params, TERM_IS_MANDATORY).getOrElse(Defaults.isMandatory)
        val abbr = extract[String](params, TERM_ABBREVIATION)
        RawArg(
          OptionArg(name = opt.syntax, abbr = abbr,
            description = description, isMandatory = isMandatory), idx, tpe
        )
    }
  }

  //todo: convert camel case name to hyphen linked
  // ============== Helpers ================
  private def extract[T: ClassTag](params: Seq[Term.Arg], paramName: Term.Name): Option[T] = {
    val ParamName = paramName
    val value = params.map {
      case named: Term.Arg.Named => named
      case unnamed =>
        abort(s"Arg definition must use named parameter in def for content:${unnamed.syntax}")
    }.collect { case Term.Arg.Named(ParamName, v) => v }.headOption
    val tpe = implicitly[ClassTag[T]]
    val actual = tpe.runtimeClass match {
      case rc if rc == classOf[String] => value collect { case Lit.String(v) => v }
      case rc if rc == classOf[Boolean] => value collect { case Lit.Boolean(v) => v }
      case rc => throw new AssertionError(s"Type not coded for argDef def: $rc")
    }
    actual.map(_.asInstanceOf[T])
  }
}