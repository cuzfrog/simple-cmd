package com.github.cuzfrog.scmd.macros.argutils

import com.github.cuzfrog.scmd.{AppInfo, Argument, BuiltInArg, Command, Defaults}
import com.github.cuzfrog.scmd.internal.RawArgMacro
import com.github.cuzfrog.scmd.macros.{TermArg, TermPrior}

import scala.collection.immutable
import scala.meta._

/*
 * Intellij code inspection is too slow, split implementation into objects.
 */

private[macros] object ArgUtils {
  /** Collect arg info from statements. */
  def collectRawArg(stats: immutable.Seq[Stat]): immutable.Seq[RawArg] =
    ArgCollectImpl.collectRawArg(stats)

  /** Generate fields of parsed arguments for newly created argDef class. */
  def convertParsed(rawArgs: immutable.Seq[RawArg]): immutable.Seq[Stat] =
    ConvertParsedImpl.convertParsed(rawArgs)

  /** Scala meta generated fields need explicit types to inform IDE. */
  def addExplicitType(rawArgs: immutable.Seq[RawArg]): immutable.Seq[Stat] =
    AddExplicitTypeImpl.addExplicitType(rawArgs)

  //todo: (low priority) make builtInArgs more generic.
  /** Used in ScmdDefMacro to generate built-in args. */
  def builtInArgs(implicit appInfo: AppInfo): immutable.Seq[TermArg] = Argument.builtInArgs.map {
    case (symbol, _) =>
      val topCmdSymbol = Lit.Symbol(Command.topCmd(appInfo.name).symbol)
      new TermPrior(symbol.name,
        q"runtime.builtInArgs(${Lit.Symbol(symbol)})", Position.None, topCmdSymbol) with BuiltInArg
  }.to[immutable.Seq]

  /** Used in ScmdDefMacro as api stub.  */
  def builtInPriorsStub: immutable.Seq[Defn.Def] = Argument.builtInArgs.map {
    case (symbol, _) =>
      q"""def ${Term.Name(symbol.name)}: PriorArg = {
            scmdRuntime.getBuiltInPrior(${Lit.Symbol(symbol)})
          }"""
  }.to[immutable.Seq]

  // ------------------- Shared helpers ----------------------
  private[argutils] def getComposedTpe(isMandatory: Boolean, hasDefault: Boolean,
                                       arg: Type, tpe: Type, argValue: Type)
                                      (implicit pos: Position): Type = {
    val argValueTpe = if (arg.structure == Types.propertyArg.structure) t"(String,$tpe)" else tpe
    val composedTpe = if (isMandatory && hasDefault) {
      throw new AssertionError("Argument cannot be mandatory while with default value.")
    } else if (isMandatory) {
      t"$arg[$tpe] with $argValue[$argValueTpe] with Mandatory"
    } else if (hasDefault) {
      t"$arg[$tpe] with $argValue[$argValueTpe] with WithDefault"
    } else {
      t"$arg[$tpe] with $argValue[$argValueTpe]"
    }
    composedTpe
  }


}

