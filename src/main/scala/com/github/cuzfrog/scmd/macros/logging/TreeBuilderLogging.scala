package com.github.cuzfrog.scmd.macros.logging

import com.github.cuzfrog.scmd.internal.{IgnoreLogging, SimpleLogging}
import com.github.cuzfrog.scmd.macros.{TermAppInfo, TermArg, TermArgTree, TermParam, TreeBuilder}

import scala.collection.immutable
import scala.meta.Term

private[macros] trait TreeBuilderLogging extends TreeBuilder with SimpleLogging {
  override protected implicit val loggerLevel = SimpleLogging.Debug

  @IgnoreLogging
  abstract override
  def buildArgTreeByDSL(appInfo: TermAppInfo,
                        argDefs: immutable.Seq[TermArg],
                        dslStats: immutable.Seq[Term.Arg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree = {
    val result = super.buildArgTreeByDSL(appInfo, argDefs, dslStats, globalLimitationsStats)
    debug(s"-Tree build--------\n" +
      s"${result.defnTerm.syntax.replaceAll("""_root_.scala.collection.immutable.""", "")}" +
      s"\n-----------------------Tree build end.--------")
    result
  }

  @IgnoreLogging
  abstract override
  def buildArgTreeByIdx(appInfo: TermAppInfo,
                        argDefs: immutable.Seq[TermArg],
                        globalLimitationsStats: immutable.Seq[Term.Arg]): TermArgTree = {
    val result = super.buildArgTreeByIdx(appInfo, argDefs, globalLimitationsStats)
    debug(s"-Tree build--------\n" +
      s"${result.defnTerm.syntax.replaceAll("""_root_.scala.collection.immutable.""", "")}" +
      s"\n-----------------------Tree build end.--------")
    result
  }

  abstract override protected
  def extractAmbiguousVariableParam(termArgTree: TermArgTree): Seq[(TermParam, TermParam)] = {

    val result = super.extractAmbiguousVariableParam(termArgTree)
    if (termArgTree.appInfo.appInfo.name.contains("optionality")) {
      termArgTree.topParams.map(p =>
        s"${p.name}|variable:${p.isVariable}|mandatory:${p.isMandatory}").foreach(debug(_))
      debug("ambiguous params:" + result.map{case (p1,p2)=> s"${p2.name} -> ${p1.name}"})
    }
    result
  }
}
