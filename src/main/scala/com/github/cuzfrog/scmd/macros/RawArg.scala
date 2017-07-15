package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.{Argument, Command, Defaults, Parameter}
import scala.collection.immutable
import scala.meta._


private final case class RawArg(arg: Argument[_], idx: Int, tpe: Type)
private object RawArg {
  def collectRawArg(stats: immutable.Seq[Stat]): immutable.Seq[RawArg] = {
    stats.zipWithIndex collect {
      case (q"val $cmd: $_ = cmdDef($dscr)", idx) =>
        val dscrContent = dscr match {
          case Term.Arg.Named(_, Lit.String(c)) => c
          case Lit.String(c) => c
        }
        RawArg(Command(cmd.syntax, dscrContent), idx, TYPE_NOTHING)

      case (q"val $para = paraDef[$tpe](..$params)", idx) =>
        val dscrCtnt = params.collect {
          case Term.Arg.Named(TERM_DESCRIPTION, Lit.String(c)) => c
          case Lit.String(c) => c
        }.headOption
        val isMandatory = params.collect {
          case Term.Arg.Named(TERM_IS_MANDATORY, Lit.Boolean(b)) => b
          case Lit.Boolean(b) => b
        }.headOption.getOrElse(Defaults.isMandatory)
        RawArg(
          Parameter(name = para.syntax, description = dscrCtnt, isMandatory = isMandatory)
          , idx, tpe
        )
    }
  }

  // ============== Helpers ================
  private implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}