package com.github.cuzfrog.scmd.macros


import com.github.cuzfrog.scmd.{Command, Defaults, OptionArg, Parameter}

import scala.meta._
import scala.collection.immutable


private[scmd] object ScmdDefMacro {
  private val TERM_NAME = Term.Name("name")
  private val TERM_DESCRIPTION = Term.Name("description")
  private val TERM_IS_MANDATORY = Term.Name("isMandatory")
  private val TYPE_NOTHING = Type.Name("Nothing")

  def expand(name: Term.Name, stats: immutable.Seq[Stat]): Defn.Object = {

    val argDefs = collectRawArg(stats)


    //reifiedArgs.foreach(t => println(t.structure))

    //    argDefs.foreach(println)
    //    println(ArgBuilder.buildArgGraphByIdx(argDefs))

    val addMethods = Seq(
      //q"val argDefs = _root_.scala.collection.Seq(..$argDefs)",
      q"def parse(args: Array[String]): Unit = { args.foreach(println) }"
    )
    val moreStats = stats ++ addMethods

    abort("dev...")

    q"object $name { ..$moreStats }"
  }


  private def collectRawArg(stats: Seq[Stat]): Seq[RawArg] = {
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

  private def raw2termArg(rawArg: RawArg): TermArg = {
    val name = Term.Name(rawArg.arg.name)
    val desciption = rawArg.arg.description match {
      case Some(dscr) => q"Option(${Lit.String(dscr)})"
      case None => q"None"
    }
    val (term, argTpe) = rawArg.arg match {
      case _: Command =>
        (q"Command($TERM_NAME = $name,$TERM_DESCRIPTION = $desciption)", ArgType.Cmd)
      case param: Parameter[_] =>
        val isMandatory = Lit.Boolean(param.isMandatory)
        val term =
          q"""Parameter[${rawArg.tpe}]($TERM_NAME = $name,
                                     $TERM_DESCRIPTION = $desciption,
                                     $TERM_IS_MANDATORY = $isMandatory)"""
        (term, ArgType.Param)
      case _: OptionArg[_] =>
        val term = q"OptionArg[${rawArg.tpe}]($TERM_NAME = $name,$TERM_DESCRIPTION = $desciption)"
        (term, ArgType.Opt)
    }
    TermArg(term, argTpe, rawArg.idx, rawArg.tpe)
  }
  // ============== Helpers ================
  implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}

