package com.github.cuzfrog.scmd.macros

import com.github.cuzfrog.scmd.Convertible
import com.github.cuzfrog.scmd.macros.Constants._

import scala.collection.immutable
import scala.meta._

private final
case class TermCmdNode(cmd: TermCmd,
                       params: immutable.Seq[TermParam],
                       opts: immutable.Seq[TermOpt],
                       priors: immutable.Seq[TermPrior],
                       subCmdEntry: TermCommandEntry,
                       limitations: immutable.Seq[LimitationGroup] = Nil)

private final
case class TermArgTree(appName: Lit.String,
                       topParams: immutable.Seq[TermParam],
                       topOpts: immutable.Seq[TermOpt],
                       topPriors: immutable.Seq[TermPrior],
                       props: immutable.Seq[TermProp],
                       cmdEntry: TermCommandEntry,
                       topLimitations: immutable.Seq[LimitationGroup] = Nil,
                       globalLimitations: immutable.Seq[LimitationGroup] = Nil)

private object TermTree {
  def collectTreeDefDsl(stats: immutable.Seq[Stat]): immutable.Seq[Term.Arg] = {
    stats.collect {
      case q"argTreeDef(..$params)" => params
    }.flatten
  }

  def collectArgGlobalLimitations(stats: immutable.Seq[Stat]): immutable.Seq[Term.Arg] = {
    stats.collect {
      case q"argDependencyDef(..$params)" => params
    }.flatten
  }
}

private object TermCmdNode {
  implicit val definable: Definable[TermCmdNode] = (a: TermCmdNode) => recDefine(a)

  private def recDefine(a: TermCmdNode): Term = {
    val entity = a.cmd.term
    val params = q"$TERM_immutable.Seq(..${a.params.map(_.defnTerm)})"
    val opts = q"$TERM_immutable.Seq(..${a.opts.map(_.defnTerm)})"
    val priors = q"$TERM_immutable.Seq(..${a.priors.map(_.defnTerm)})"
    val subCmdEntry = a.subCmdEntry.defnTerm
    val limitations = q"$TERM_immutable.Seq(..${a.limitations.map(_.defnTerm)})"

    q"""runtime.buildCmdNode(
          entity = $entity,
          params = $params,
          opts = $opts,
          priors = $priors,
          subCmdEntry = $subCmdEntry,
          limitations = $limitations
        )"""
  }
}

private object TermArgTree {
  implicit val definable: Definable[TermArgTree] = (a: TermArgTree) => {
    val topParams = a.topParams.map(_.defnTerm)
    val topOpts = a.topOpts.map(_.defnTerm)
    val topPriors = a.topPriors.map(_.defnTerm)
    val props = a.props.map(_.defnTerm)
    val cmdEntry = a.cmdEntry.defnTerm
    val topLimitations = q"$TERM_immutable.Seq(..${a.topLimitations.map(_.defnTerm)})"
    val globalLimitations = q"$TERM_immutable.Seq(..${a.globalLimitations.map(_.defnTerm)})"
    q"""runtime.buildArgTree(
          appName = ${a.appName},
          topParams = $TERM_immutable.Seq(..$topParams),
          topOpts = $TERM_immutable.Seq(..$topOpts),
          topPriors = $TERM_immutable.Seq(..$topPriors),
          props = $TERM_immutable.Seq(..$props),
          cmdEntry = $cmdEntry,
          topLimitations = $topLimitations,
          globalLimitations = $globalLimitations
        )"""
  }

  implicit val convert2NodeSeq: Convertible[TermArgTree, immutable.Seq[TermArg]] =
    (a: TermArgTree) => {
      def recConvertTermCmdNode2NodeSeq(tn: TermCmdNode): immutable.Seq[TermArg] = {
        tn.params ++ tn.opts ++
          tn.subCmdEntry.children.flatMap(recConvertTermCmdNode2NodeSeq) :+ tn.cmd
      }
      a.props ++ a.topParams ++ a.topOpts ++
        a.cmdEntry.children.flatMap(recConvertTermCmdNode2NodeSeq)
    }
}