package com.github.cuzfrog.scmd.macros.logging

import com.github.cuzfrog.scmd.internal.{IgnoreLogging, SimpleLogging}
import com.github.cuzfrog.scmd.macros.{TermArg, TermArgTree, TreeBuilder}

import scala.collection.immutable
import scala.meta.Term

private[macros] trait TreeBuilderLogging extends TreeBuilder with SimpleLogging {
  override protected implicit val loggerLevel = SimpleLogging.Debug

  @IgnoreLogging
  abstract override
  def buildArgTreeByDSL(appName: String,
                        argDefs: immutable.Seq[TermArg],
                        dslStats: immutable.Seq[Term.Arg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree = {
    val result = super.buildArgTreeByDSL(appName, argDefs, dslStats, globalLimitationsStats)
    debug(s"-Tree build--------\n" +
      s"${result.defnTerm.syntax.replaceAll("""_root_.scala.collection.immutable.""", "")}" +
      s"\n-----------------------Tree build end.--------")
    result
  }

  @IgnoreLogging
  abstract override
  def buildArgTreeByIdx(appName: String,
                        argDefs: immutable.Seq[TermArg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree = {
    val result = super.buildArgTreeByIdx(appName, argDefs, globalLimitationsStats)
    debug(s"-Tree build--------\n" +
      s"${result.defnTerm.syntax.replaceAll("""_root_.scala.collection.immutable.""", "")}" +
      s"\n-----------------------Tree build end.--------")
    result
  }
}
