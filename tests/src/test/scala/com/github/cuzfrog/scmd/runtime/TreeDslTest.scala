package com.github.cuzfrog.scmd.runtime

import Scmd._
import com.github.cuzfrog.scmd.ScmdDefTest
import org.junit.Test


class TreeDslTest {

  private val defClass = new TreeDslDef(Nil)
  private val tree: ArgTree = defClass.getRuntime.getArgTree
  @Test
  def test1(): Unit = {
    assert(tree.topParams.map(_.entity.name) == Seq("param0"))
    assert(tree.topOpts.map(_.entity.name) == Seq("opt0"))
    assert(tree.cmdEntry.children.map(_.entity.name) == Seq("cmd1", "cmd2"))
    val cmd1 = tree.cmdEntry.children.find(_.entity.name == "cmd1").get
    val cmd12 = cmd1.subCmdEntry.children.find(_.entity.name == "cmd2").get
    assert(cmd1.opts.isEmpty && cmd1.params.isEmpty)
    assert(cmd12.opts.map(_.entity.name) == Seq("opt2"))
    assert(cmd12.params.map(_.entity.name) == Seq("param2"))
    val cmd2 = tree.cmdEntry.children.find(_.entity.name == "cmd2").get
    assert(cmd2.opts.map(_.entity.name) == Seq("opt2"))
    assert(cmd2.params.map(_.entity.name) == Seq("param2"))
    val cmd23 = cmd2.subCmdEntry.children.find(_.entity.name == "cmd3").get
    assert(cmd2.subCmdEntry.children.size == 1)
    assert(cmd23.opts.map(_.entity.name) == Seq("opt3"))
    assert(cmd23.params.isEmpty)
    assert(tree.props.map(_.entity.flag) == Seq("D"))
    assert(tree.priors.map(_.entity.name).contains("globalPrior"))
  }

  @ScmdDefTest
  private class TreeDslDef(args: Seq[String]) extends ScmdDefStub[TreeDslDef] {

    val cmd1 = cmdDef()
    val cmd2 = cmdDef()
    val cmd3 = cmdDef()

    val opt0 = optDef[Boolean]()
    val opt2 = optDef[Boolean]()
    val opt3 = optDef[Boolean]()

    val param0 = paramDef[Long]()
    val param2 = paramDef[Long]()

    val globalPrior = priorDef(matchName = true)
    val globalProp = propDef[Int](flag = "D")

    import scmdTreeDefDSL._

    argTreeDef(
      param0,
      opt0,
      cmd1(
        cmd2(opt2, param2)
      ),
      cmd2(
        param2,
        cmd3(opt3),
        opt2
      )
    )
  }
}
