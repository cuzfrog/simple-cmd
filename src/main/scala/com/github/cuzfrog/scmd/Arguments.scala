package com.github.cuzfrog.scmd

/**
  * Exposed to macros for basic validation, so need to put in this package.
  */
sealed trait Argument[+T] {
  def name: String
  def description: Option[String]
  def symbol: scala.Symbol = scala.Symbol(name)
}


sealed case class
Command private[scmd](name: String,
                      description: Option[String] = None,
                      private[scmd] val met: Boolean = false) extends Argument[Nothing]


sealed case class
CommandEntry private[scmd](isMandatory: Boolean = Defaults.isMandatory)

sealed trait ValueArgument[+T] extends Argument[T]

sealed case class
Parameter[+T] private[scmd](name: String,
                            description: Option[String] = None,
                            isMandatory: Boolean = Defaults.isMandatory) extends ValueArgument[T] {
}


sealed case class
OptionArg[+T] private[scmd](name: String,
                            abbr: Option[String] = None,
                            description: Option[String] = None,
                            isMandatory: Boolean = Defaults.isMandatory) extends ValueArgument[T] {

}

sealed trait ArgValue[+T] {
  private[scmd] def isVariable: Boolean
}

sealed trait SingleValue[+T] extends ArgValue[T] {
  private[scmd] def value: Option[T] = None
  private[scmd] def default: Option[T] = None
  private[scmd] def isVariable: Boolean = false
}
sealed trait VariableValue[+T] extends ArgValue[T] {
  private[scmd] def value: Seq[T] = Nil
  private[scmd] def default: Seq[T] = Nil
  private[scmd] def isVariable: Boolean = true
}

sealed trait Mandatory

private[scmd] object DummyCommand extends Command("")
private[scmd] object DummyParameterS extends Parameter("") with SingleValue[Nothing]
private[scmd] object DummyParameterV extends Parameter("") with VariableValue[Nothing]
private[scmd] object DummyOptionArgS extends OptionArg("") with SingleValue[Nothing]
private[scmd] object DummyOptionArgV extends OptionArg("") with VariableValue[Nothing]
private[scmd] object DummyParameterSM extends Parameter("") with SingleValue[Nothing] with Mandatory
private[scmd] object DummyParameterVM extends Parameter("") with VariableValue[Nothing] with Mandatory
private[scmd] object DummyOptionArgSM extends OptionArg("") with SingleValue[Nothing] with Mandatory
private[scmd] object DummyOptionArgVM extends OptionArg("") with VariableValue[Nothing] with Mandatory

//todo: use macros to eliminate boilerplate:
private object Parameter {
  private[scmd] implicit def mergeValue[T]: CanMerge[Parameter[T], Seq[T]] =
    (a: Parameter[T], stuff: Seq[T]) => {
      val result = a match {
        case arg: SingleValue[T@unchecked] =>
          createParam[T](a.name, a.description, arg.isMandatory, stuff.headOption, arg.default)
        case arg: VariableValue[T@unchecked] =>
          createParam[T](a.name, a.description, arg.isMandatory, stuff, arg.default)
        case arg: Parameter[T] => throw new AssertionError(s"Nude arg:$arg cannot merge a value.")
      }
      result.asInstanceOf[Parameter[T]]
    }

  private[scmd] implicit def mixValueTrait
  [T, V <: ArgValue[T]]: CanMix[Parameter[T], V] =
    (a: Parameter[T], stuff: V) => {
      val result = stuff match {
        case s: SingleValue[T@unchecked] =>
          createParam[T](a.name, a.description, a.isMandatory, s.value, s.default)
        case m: VariableValue[T@unchecked] =>
          createParam[T](a.name, a.description, a.isMandatory, m.value, m.default)
      }
      result.asInstanceOf[Parameter[T] with V]
    }


  private def createParam[T](name: String, description: Option[String],
                             isMandatory: Boolean, _value: Option[T], _default: Option[T]) = {
    if (isMandatory) {
      new Parameter[T](name = name, description = description, isMandatory = isMandatory)
        with SingleValue[T] with Mandatory {
        override def value: Option[T] = _value
        override def default: Option[T] = _default
      }
    } else {
      new Parameter[T](name = name, description = description, isMandatory = isMandatory)
        with SingleValue[T] {
        override def value: Option[T] = _value
        override def default: Option[T] = _default
      }
    }
  }
  private def createParam[T](name: String, description: Option[String],
                             isMandatory: Boolean, _value: Seq[T], _default: Seq[T]) = {
    if (isMandatory) {
      new Parameter[T](name = name, description = description, isMandatory = isMandatory)
        with VariableValue[T] with Mandatory {
        override def value: Seq[T] = _value
        override def default: Seq[T] = _default
      }
    } else {
      new Parameter[T](name = name, description = description, isMandatory = isMandatory)
        with VariableValue[T] {
        override def value: Seq[T] = _value
        override def default: Seq[T] = _default
      }
    }

  }
}

private object OptionArg {
  private[scmd] implicit def mergeValue[T]: CanMerge[OptionArg[T], Seq[T]] =
    (a: OptionArg[T], stuff: Seq[T]) => {
      val result = a match {
        case arg: SingleValue[T@unchecked] =>
          createOpt[T](a.name, arg.abbr, a.description, arg.isMandatory, stuff.headOption, arg.default)
        case arg: VariableValue[T@unchecked] =>
          createOpt[T](a.name, arg.abbr, a.description, arg.isMandatory, stuff, arg.default)
        case arg: OptionArg[T] => throw new AssertionError(s"Nude arg:$arg cannot merge a value.")
      }
      result.asInstanceOf[OptionArg[T]]
    }

  private[scmd] implicit def mixValueTrait
  [T, V <: ArgValue[T], A <: ValueArgument[T]]: CanMix[OptionArg[T], V] =
    (a: OptionArg[T], stuff: V) => {
      val result = stuff match {
        case s: SingleValue[T@unchecked] =>
          createOpt[T](a.name, a.abbr, a.description, a.isMandatory, s.value, s.default)

        case m: VariableValue[T@unchecked] =>
          createOpt[T](a.name, a.abbr, a.description, a.isMandatory, m.value, m.default)
      }
      result.asInstanceOf[OptionArg[T] with V]
    }


  private def createOpt[T](name: String, abbr: Option[String], description: Option[String],
                           isMandatory: Boolean, _value: Option[T], _default: Option[T]) = {
    if (isMandatory) {
      new OptionArg[T](name = name, abbr = abbr, description = description, isMandatory = isMandatory)
        with SingleValue[T] with Mandatory {
        override def value: Option[T] = _value
        override def default: Option[T] = _default
      }
    } else {
      new OptionArg[T](name = name, abbr = abbr, description = description, isMandatory = isMandatory)
        with SingleValue[T] {
        override def value: Option[T] = _value
        override def default: Option[T] = _default
      }
    }

  }
  private def createOpt[T](name: String, abbr: Option[String], description: Option[String],
                           isMandatory: Boolean, _value: Seq[T], _default: Seq[T]) = {
    if (isMandatory) {
      new OptionArg[T](name = name, abbr = abbr, description = description, isMandatory = isMandatory)
        with VariableValue[T] with Mandatory {
        override def value: Seq[T] = _value
        override def default: Seq[T] = _default
      }
    } else {
      new OptionArg[T](name = name, abbr = abbr, description = description, isMandatory = isMandatory)
        with VariableValue[T] {
        override def value: Seq[T] = _value
        override def default: Seq[T] = _default
      }
    }

  }
}

private object ValueArgument {
  implicit def mergeValue
  [T, V <: ArgValue[T]]: CanMerge[ValueArgument[T], Seq[T]] =
    (a: ValueArgument[T], stuff: Seq[T]) => {
      val result = a match {
        case p: Parameter[T] => merge(p, stuff)
        case o: OptionArg[T] => merge(o, stuff)
      }
      result.asInstanceOf[ValueArgument[T] with V]
    }
}

private object ArgValue {
  private[scmd] def single[T](_default: Option[T]): ArgValue[T] = new SingleValue[T] {
    override def default: Option[T] = _default
    override def value: Option[T] = None
  }
  private[scmd] def variable[T](_default: Seq[T]): ArgValue[T] = new VariableValue[T] {
    override def default: Seq[T] = _default
    override def value: Seq[T] = Nil
  }
}