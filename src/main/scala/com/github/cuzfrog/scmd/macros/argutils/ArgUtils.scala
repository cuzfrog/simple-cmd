package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.{Argument, Defaults}
import com.github.cuzfrog.scmd.internal.RawArgMacro

import scala.collection.immutable
import scala.meta._

/*
 * Intellij code inspection is too slow, split implementation into objects.
 */

private[macros] object ArgUtils {
  /** Generate fields of parsed arguments for newly created argDef class. */
  def convertParsed(stats: immutable.Seq[Stat]): immutable.Seq[Stat] =
    ConvertParsedImpl.convertParsed(stats)

  /** Scala meta generated fields need explicit types to inform IDE. */
  def addExplicitType(stat: Stat): Stat = AddExplicitTypeImpl.addExplicitType(stat)

  def builtInPriorsStub: immutable.Seq[Defn.Def] = Argument.builtInArgs.map {
    case (symbol, _) =>
      q"""def ${Term.Name(symbol.name)}: PriorArg = {
            scmdRuntime.getBuiltInPrior(${Lit.Symbol(symbol)})
          }"""
  }.to[immutable.Seq]

  def extractSelection(stat: Stat): Seq[Selection] = {
    val selections = stat match {
      case q"val $argName: $_ = $defName(..$params).$s1" => Seq(s1)
      case q"val $argName: $_ = $defName(..$params).$s1.$s2" => Seq(s1, s2)
      case _ => Nil
    }
    selections.map {
      case q"mandatory" => Selection.Mandatory
      case q"withDefault($param)" =>
        val value = param match {
          case Term.Arg.Named(_, v) => v
          case v => v
        }
        Selection.WithDefault(value)
    }
  }

  sealed trait Selection
  object Selection {
    case object Mandatory extends Selection
    case class WithDefault(v: Term.Arg) extends Selection
  }

  // ------------------- Shared helpers ----------------------
  private[argutils] def getComposedTpe(params: immutable.Seq[Term.Arg],
                                       arg: Type,
                                       tpe: Type,
                                       argValue: Type)
                                      (implicit pos: Position): (Boolean, Boolean, Type) = {
    val isMandatory = RawArgMacro.extract[Boolean](params).getOrElse(Defaults.isMandatory)
    val withDefault = {
      val default = RawArgMacro.extract[Term.Arg](params)
      if (default.isEmpty && tpe.syntax == "Boolean" &&
        argValue.structure == Types.singleValue.structure) true
      else default.nonEmpty
    }
    val argValueTpe = if (arg.structure == Types.propertyArg.structure) t"(String,$tpe)" else tpe
    val composedTpe = if (isMandatory && withDefault) {
      throw new AssertionError("Argument cannot be mandatory while with default value.")
    } else if (isMandatory) {
      t"$arg[$tpe] with $argValue[$argValueTpe] with Mandatory"
    } else if (withDefault) {
      t"$arg[$tpe] with $argValue[$argValueTpe] with WithDefault"
    } else {
      t"$arg[$tpe] with $argValue[$argValueTpe]"
    }
    (isMandatory, withDefault, composedTpe)
  }


}

