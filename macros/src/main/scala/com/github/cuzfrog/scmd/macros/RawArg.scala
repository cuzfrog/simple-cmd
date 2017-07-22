package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.internal.RawArgMacro
import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.{Argument, Command, Defaults, OptionArg, Parameter}

import scala.collection.immutable
import scala.meta._

private final case class RawArg(arg: Argument[_], pos: Position, tpe: Type)

private object RawArg {
  def collectRawArg(stats: immutable.Seq[Stat]): immutable.Seq[RawArg] = {
    stats zip stats.map(_.pos) collect {
      case (q"val $cmd: $_ = cmdDef(..$params)", pos) =>
        implicit val position = pos
        val description = RawArgMacro.extract[String](params)
        RawArg(Command(cmd.syntax, description), pos, TYPE_NOTHING)

      case (q"val $para: $_ = paramDef[$tpe](..$params)", pos) =>
        implicit val position = pos
        val description = RawArgMacro.extract[String](params)
        val isMandatory =
          RawArgMacro.extract[Boolean](params).getOrElse(Defaults.isMandatory)
        RawArg(
          Parameter(name = para.syntax, description = description, isMandatory = isMandatory)
          , pos, tpe
        )

      case (q"val $opt: $_ = optDef[$tpe](..$params)", pos) =>
        implicit val position = pos
        val description = RawArgMacro.extract[String](params)
        val isMandatory =
          RawArgMacro.extract[Boolean](params).getOrElse(Defaults.isMandatory)
        val abbr = RawArgMacro.extract[String](params)
        RawArg(
          OptionArg(name = opt.syntax, abbr = abbr,
            description = description, isMandatory = isMandatory), pos, tpe
        )
    }
  }

  //todo: convert camel case name to hyphen linked
}