package com.github.cuzfrog.scmd


import scala.meta.Type
import scala.reflect.ClassTag

/**
  * Created by cuz on 7/13/17.
  */
private final class ArgBuilder {
  def buildArgTree(argDefs: Seq[RawArg]): Unit = {

  }
}

private final case class RawArg(arg: Argument[_], idx: Int, tpe: Type)
