package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.CanFormPrettyString

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


  implicit val nodeSeqCanFormPrettyString: CanFormPrettyString[Seq[Node]] = (a: Seq[Node]) => {
    a.map {
      case n: CmdNode => s"cmd:${n.entity.name}"
      case n: ParamNode[_] =>
        val ifVariable = if (n.isVariable) "..." else ""
        s"param$ifVariable: ${n.entity.name}[${n.tpe}] = ${n.value}"
      case n: OptNode[_] => s"opt: ${n.entity.name}[${n.tpe}] = ${n.value}"
      case n: CmdEntryNode =>
        throw new AssertionError(s"CmdEntryNode should not be parsed.$n")
    }.mkString(System.lineSeparator)
  }
}
