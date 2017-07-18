package com.github.cuzfrog.scmd.parse

private object ArgTreeUtils {

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

  def countMandatory(cmdNode: CmdNode): Int = {
    val paramCnt = cmdNode.params.count(_.entity.isMandatory)
    paramCnt + countMandatory(cmdNode.subCmdEntry)
  }

  def countMandatory(cmdEntryNode: CmdEntryNode): Int = {
    if(!cmdEntryNode.entity.isMandatory) 0
    else cmdEntryNode.children.map(countMandatory).sum
  }
}
