package com.github.cuzfrog.scmd.parse

private case class AppInfo(name: String,
                           shortDescription: Option[String] = None,
                           fullDescription: Option[String] = None,
                           version: Option[String] = None,
                           license: Option[String] = None,
                           author: Option[String] = None,
                           custom: Map[String, String] = Map.empty)
