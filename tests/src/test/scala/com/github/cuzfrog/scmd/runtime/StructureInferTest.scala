package com.github.cuzfrog.scmd.runtime

import Scmd._
import com.github.cuzfrog.scmd.ScmdDefTest
import org.junit._
import org.junit.Assert._

class StructureInferTest {
  private val defClass = new StructureInferDef(Nil)
  private val tree: ArgTree = defClass.getRuntime.getArgTree
  @Test
  def test1(): Unit = {
    assertTrue("name infer", tree.appInfo.name == "StructureInfer".toLowerCase)
    assertTrue("global prop", tree.props.map(_.entity.name).contains("globalProp"))
    assertTrue("global priors",
      tree.priors.map(_.entity.name) == Seq("help", "version", "globalPrior1", "globalPrior2"))
    assertTrue("top param shoud be empty", tree.topParams.isEmpty)
    assertTrue("top opt", tree.topOpts.map(_.entity.name) == Seq("topOpt"))
    assertTrue("cmds", tree.cmdEntry.children.map(_.entity.name) == Seq("cmd1", "cmd2", "cmd3"))
    assertTrue("sharedparam",
      tree.cmdEntry.children.map(_.params.map(_.entity.name)).forall(_.contains("sharedParam"))
    )
    assertTrue("opt2",
      tree.cmdEntry.children.filter(_.entity.name == "cmd2")
        .exists(_.opts.map(_.entity.name) == Seq("opt2")))
    assertTrue("param3",
      tree.cmdEntry.children.filter(_.entity.name == "cmd3")
        .exists(_.params.map(_.entity.name).contains("param3")))
  }

  @ScmdDefTest
  private class StructureInferDef(args: Seq[String]) extends ScmdDefStub[StructureInferDef] {
    val sharedParam = paramDef[String](description = "this should be shared by cmds below.")
    val globalPrior1 = priorDef(alias = Seq("--global1"))
    val topOpt = optDef[Int]()
    val cmd1 = cmdDef()
    val globalProp = propDef[Int](flag = "D")
    val cmd2 = cmdDef()
    val opt2 = optDef[Boolean]()
    val cmd3 = cmdDef()
    val param3 = paramDef[Long]()
    val globalPrior2 = priorDef(matchName = true)
  }
}

