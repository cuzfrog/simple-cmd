package com.github.cuzfrog.scmd.runtime


private trait StringUtils {
  protected implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}
