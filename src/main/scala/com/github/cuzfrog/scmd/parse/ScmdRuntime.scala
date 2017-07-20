package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.Defaults

import scala.reflect.ClassTag

sealed trait ScmdRuntime {
  def addAppInfo(name: String,
                 shortDescription: Option[String] = None,
                 fullDescription: Option[String] = None,
                 version: Option[String] = None,
                 license: Option[String] = None,
                 author: Option[String] = None,
                 custom: Map[String, String] = Map.empty): this.type

  def buildCommand(name: String,
                   description: Option[String]): Int

  def buildParameter[T](name: String,
                        description: Option[String] = None,
                        isMandatory: Boolean = Defaults.isMandatory,
                        default: Option[T] = None): Int

  def buildOptionArg[T](name: String,
                        abbr: Option[String] = None,
                        description: Option[String] = None,
                        isMandatory: Boolean = Defaults.isMandatory,
                        default: Option[T] = None): Int

  def buildParamNode[T](entity: Int,
                        tpe: ClassTag[T],
                        value: Seq[String]): Int

  def buildOptNode[T](entity: Int,
                      tpe: ClassTag[T],
                      value: Seq[String]): Int

  def buildCmdEntryNode(entity: Int,
                        children: Seq[Int]): Int

  def defaultCommandEntry: Int

  def buildCmdNode(entity: Int,
                   params: Seq[Int],
                   opts: Seq[Int],
                   parent: Option[Int],
                   subCmdEntry: Int)

  def buildArgTree(topParams: Seq[Int],
                   topOpts: Seq[Int],
                   cmdEntry: Int): this.type
}
object ScmdRuntime {
  def createBuilder: ScmdRuntime = new ScmdRuntimeImpl
}

private class ScmdRuntimeImpl extends ScmdRuntime {

}