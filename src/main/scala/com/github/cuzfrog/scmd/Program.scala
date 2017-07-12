package com.github.cuzfrog.scmd

/**
  * Created by cuz on 17-7-12.
  */
final case class Program(name: String,
                         shortDescription: Option[String] = None,
                         version: Option[String] = None,
                         author: Option[String] = None)