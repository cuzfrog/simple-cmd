package com.github.cuzfrog.scmd.parse

private[parse] trait ArgTreeUtils {

  implicit class ArgTreeOps(a: ArgTree) {
    //    /** Convert this immutable ArgTree to a stateful tree for further operation. */
    //    def toStateTree: StateCmdNode = {
    //      def recMakeStateful(cmdNode: CmdNode): StateCmdNode = {
    //        require(cmdNode.parent.isEmpty, "Stateful conversion can only be done one top cmd node.")
    //        val params = cmdNode.params.map(StateParamNode)
    //        val opts = cmdNode.opts.map(StateOptNode)
    //        val subCmdEntry =
    //          StateCmdEntryNode(ref = cmdNode.subCmdEntry,
    //            children = cmdNode.subCmdEntry.children.map(recMakeStateful))
    //        StateCmdNode(ref = cmdNode, params = params, opts = opts,
    //          parent = None, subCmdEntry = subCmdEntry)
    //      }
    //      recMakeStateful(a.toTopNode)
    //    }
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
