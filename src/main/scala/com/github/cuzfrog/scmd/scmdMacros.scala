package com.github.cuzfrog.scmd

import scala.collection.immutable
import scala.meta._
import scala.reflect.ClassTag

class ScmdDef extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods object $name { ..$stats }" =>
        ScmdDefMacroImpl.expand(name, stats)
      case _ =>
        abort("@ScmdDef must annotate an object.")
    }
  }
}

private object ScmdDefMacroImpl {
  private val TERM_DESCRIPTION = Term.Name("description")
  private val TERM_IS_MANDATORY = Term.Name("isMandatory")
  private val TYPE_NOTHING = Type.Name("Nothing")

  def expand(name: Term.Name, stats: immutable.Seq[Stat]): Defn.Object = {

    val argDefs = stats.zipWithIndex collect {
      case (q"val $cmd: $_ = cmdDef($dscr)", idx) =>
        val dscrCtnt = dscr match {
          case Term.Arg.Named(_, Lit.String(c)) => c
          case Lit.String(c) => c
        }
        RawArg(Command(cmd.syntax, dscrCtnt), idx, TYPE_NOTHING)

      case (q"val $para = paraDef[$tpe](..$params)", idx) => para
        val dscrCtnt = params.collect {
          case Term.Arg.Named(TERM_DESCRIPTION, Lit.String(c)) => c
          case Lit.String(c) => c
        }.headOption
        val isMandatory = params.collect {
          case Term.Arg.Named(TERM_IS_MANDATORY, Lit.Boolean(b)) => b
          case Lit.Boolean(b) => b
        }.headOption.getOrElse(Defaults.isMandatory)
        println(tpe.structure)
        RawArg(Parameter(description = dscrCtnt, isMandatory = isMandatory), idx, tpe)

    }

    argDefs.foreach(println)
    println(ArgBuilder.buildArgGraphByIdx(argDefs))

    val addMethods = Seq(
      //q"val argDefs = _root_.scala.collection.Seq(..$argDefs)",
      q"def parse(args: Array[String]): Unit = { args.foreach(println) }"
    )
    val moreStats = stats ++ addMethods

    abort("dev...")

    q"object $name { ..$moreStats }"
  }


  def printRaw(raw: RawArg): Unit = {

  }

  // ============== Helpers ================
  implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}

