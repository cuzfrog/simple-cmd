package com.github.cuzfrog.scmd

/**
  * Created by cuz on 17-7-12.
  */
trait Argument[A] {

}

class Command(name: String, description: Option[String]) {

}

object DummyCommand extends Command("", None)

class Parameter[T](description: Option[String] = None, isMandatory: Boolean = false)(scope: Command*)

object DummyParameter extends Parameter[Nothing]()()

class OptionArg[T](name: Option[String] = None,
                   description: Option[String] = None)
                  (scope: Command*) {

}

object DummyOptionArg extends OptionArg[Nothing]()()