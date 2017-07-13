package com.github.cuzfrog.scmd


private final case class ProgramInfo(name: String,
                                     shortDescription: Option[String] = None,
                                     version: Option[String] = None,
                                     author: Option[String] = None)

