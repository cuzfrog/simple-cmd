package com.github.cuzfrog.scmd

/**
  * Created by cuz on 17-7-12.
  */
trait Argument[+A]
object Argument{

}

case class Command(name: String, description: Option[String]) extends Argument[Nothing]

object DummyCommand extends Command("", None)

case class Parameter[T](description: Option[String] = None,
                        isMandatory: Boolean = Defaults.isMandatory)
  extends Argument[T]

object DummyParameter extends Parameter[Nothing]()

case class OptionArg[T](name: String, description: Option[String] = None)
  extends Argument[T]

object DummyOptionArg extends OptionArg[Nothing]("")


private object Defaults{
  val isMandatory: Boolean = false
}