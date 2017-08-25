package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.ScmdUtils._

private[runtime] trait ArgTreeUtils {

  implicit class ArgTreeOps(a: ArgTree) {

  }

  implicit class CmdNodeOps(a: CmdNode) {
    def getMandatoriesDownstream: Seq[Node] = {
      val params: Seq[Node] = a.params.filter(_.entity.isMandatory)
      val opts: Seq[Node] = a.opts.filter(_.entity.isMandatory)
      (params ++ opts ++ a.subCmdEntry.getMandatoriesDownstream) :+ a
    }

    def countMandatoryDownstream: Int = {
      val paramCnt = a.params.count(_.entity.isMandatory)
      val optCnt = a.opts.count(_.entity.isMandatory)
      paramCnt + optCnt + a.subCmdEntry.countMandatoryDownstream
    }
  }

  implicit class CmdEntryNodeOps(a: CmdEntryNode) {
    def getMandatoriesDownstream: Seq[Node] = {
      if (!a.entity.isMandatory) Nil
      else a.children.flatMap(_.getMandatoriesDownstream)
    }

    def countMandatoryDownstream: Int = {
      if (!a.entity.isMandatory) 0
      else a.children.map(_.countMandatoryDownstream).sum + 1 //1 = cmd itself
    }
  }


  implicit val nodeSeqCanFormPrettyString: CanFormPrettyString[Seq[Node]] =
    new CanFormPrettyString[Seq[Node]] {
      override def mkPrettyString(a: Seq[Node]): String = {
        a.map(_.prettyString).mkString(System.lineSeparator)
      }
    }
}
