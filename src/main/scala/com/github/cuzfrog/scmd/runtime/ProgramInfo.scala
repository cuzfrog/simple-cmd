package com.github.cuzfrog.scmd.runtime

private final case class ProgramInfo(name: String,
                                     shortDescription: Option[String] = None,
                                     version: Option[String] = None,
                                     author: Option[String] = None)

