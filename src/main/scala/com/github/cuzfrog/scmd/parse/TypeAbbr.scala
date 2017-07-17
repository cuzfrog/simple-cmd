package com.github.cuzfrog.scmd.parse

/**
  * Created by cuz on 7/17/17.
  */
private trait TypeAbbr{
  type AnchorEither = Either[ArgParseException, Seq[ValueAnchor]]
}
