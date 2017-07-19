package com.github.cuzfrog.scmd.parse

/**
  * Created by cuz on 7/19/17.
  */
class BacktrackingParserTest {

}


private sealed trait BacktrackingParserTestAddOn {
  self: BacktrackingParser =>

  def parsedToPath: Path = {
    this.proceed()
  }
}