package com.github.cuzfrog.scmd.runtime

private[runtime] trait ArgTreeUtils {

  implicit class ArgTreeOps(a: ArgTree) {

  }

  implicit class CmdNodeOps(a: CmdNode) {
    def countMandatoryDownstream: Int = {
      val paramCnt = a.params.count(_.entity.isMandatory)
      val optCnt = a.opts.count(_.entity.isMandatory)
      paramCnt + optCnt + a.subCmdEntry.countMandatoryDownstream
    }


  }

  implicit class CmdEntryNodeOps(a: CmdEntryNode) {
    def countMandatoryDownstream: Int = {
      if (!a.entity.isMandatory) 0
      else a.children.map(_.countMandatoryDownstream).sum
    }
  }


}
