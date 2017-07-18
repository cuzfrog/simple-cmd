package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{ArgTree, CmdNode}

/**
  * Created by cuz on 17-7-17.
  */
private[parse] trait ArgTreeUtils {

  implicit class CmdNodeOps(a: CmdNode) {

  }

  implicit class ArgTreeOps(a: ArgTree) {
    /** Convert this immutable ArgTree to a stateful tree for further operation. */
    def toStateTree: StateCmdNode = {
      def recMakeStateful(cmdNode: CmdNode): StateCmdNode = {
        require(cmdNode.parent.isEmpty, "Stateful conversion can only be done one top cmd node.")
        val params = cmdNode.params.map(StateParamNode)
        val opts = cmdNode.opts.map(StateOptNode)
        val subCmdEntry =
          StateCmdEntryNode(ref = cmdNode.subCmdEntry,
            children = cmdNode.subCmdEntry.children.map(recMakeStateful))
        StateCmdNode(ref = cmdNode, params = params, opts = opts,
          parent = None, subCmdEntry = subCmdEntry)
      }
      recMakeStateful(a.toTopNode)
    }
  }

  implicit class StateCmdNodeOps(a: StateCmdNode){
    
  }
}
