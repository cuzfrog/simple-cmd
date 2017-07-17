package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{OptNode, OptionArg}

import scala.reflect.ClassTag

/** Opt(s) with single letter. "-" has been stripped off. */
private case class SingleOpts(arg: String, context: Context)
/** Opt with full name. One "-" of the "--" has been stripped off. */
private case class LongOpt(arg: String, context: Context)
/** Param or Cmd with no prefix "-". */
private case class ParamOrCmd(arg: String, context: Context)


private object SingleOpts extends TypeAbbr with CateUtils {
  private val EqualLitertal = """\w=(\w+)""".r
  private val ValueFolding = """\w([^\=]{1}.*)""".r

  implicit val parser: Parser[SingleOpts, AnchorEither] = (a: SingleOpts) => {
    val cmdNode = a.context.getCurrentCmdNode
    val arg = a.arg

    val matchOpt = cmdNode.opts.find(_.entity.abbr.exists(_.head == arg.head)) //match first letter
    matchOpt match {
      case Some(optNode1) => optNode1.tpe match {
        //found argDef for type Boolean
        case ClassTag.Boolean =>
          arg match {
            //arg=literal
            case EqualLitertal(bool) =>
              parseBoolStr(bool) match {
                case Some(b) => Seq(ValueAnchor(optNode1.copy(value = Some(b.toString)), a.context))
                case None => ArgParseException(s"Unknown bool literal: $bool", a.context)
              }

            //folded letters
            case bools if bools.matches("""\w+""") =>
              val boolSet = bools.split("").toSet
              /** Nodes with optDefs matched with args with type Boolean. */
              val boolNodes = cmdNode.opts.collectWithType[OptNode[Boolean]]
                .filter(n => n.entity.abbr.exists(boolSet.contains))
              if (boolNodes.size < boolSet.size) {
                val badArgs =
                  boolSet.filterNot(s => boolNodes.flatMap(_.entity.abbr).contains(s)).mkString
                ArgParseException(s"Boolean options:[$badArgs] not defined", a.context)
              }
              else if (boolSet.size < bools.length) {
                ArgParseException(s"Duplicates in boolean options: $bools", a.context)
              }
              else {
                val optNodesWithValue = boolNodes.map { n =>
                  val boolValue = n.entity.default.getOrElse(false).unary_!.toString
                  ValueAnchor(n.copy(value = Some(boolValue)), a.context)
                }
                optNodesWithValue
              }

            case bad =>
              ArgParseException(s"Boolean opts[$bad] contain unsupported letter", a.context)
          }

        //found argDef for other types
        case otherTpe =>
          try {
            val value = arg match {
              case EqualLitertal(v) => v
              case ValueFolding(v) => v
              case single if single.matches("""\w""") => a.context.nextArg.getOrElse(
                throw ArgParseException(
                  s"No value found for opt[$arg] with type[${otherTpe.name}].", a.context))
              case bad => throw ArgParseException(s"Malformed opt[$bad]", a.context)
            }
            Seq(ValueAnchor(optNode1.copy(value = Some(value)), a.context))
          } catch {
            case e: ArgParseException => e
          }
      }
      case None =>
        ArgParseException(s"Unknown opt[$arg]", a.context)
    }
  }
}

private object LongOpt extends TypeAbbr with CateUtils {
  private val EqualLiteral = """-([\-\w\d]+)(=.*)?""".r

  implicit val parser: Parser[LongOpt, AnchorEither] = (a: LongOpt) => {
    val cmdNode = a.context.getCurrentCmdNode
    val arg = a.arg

    arg match {
      case EqualLiteral(name, e_Value) =>
        val valueOpt = Option(e_Value).map(_.drop(1))
        val matchOpt = cmdNode.opts.find(_.entity.name == name)
        matchOpt match {
          case Some(optNode) => optNode.tpe match {
            //arg def of type Boolean
            case ClassTag.Boolean =>
              val boolValue = valueOpt match {
                case Some(boolStr) =>
                  parseBoolStr(boolStr) match {
                    case Some(b) => b
                    case None => ArgParseException(s"Unknown bool literal: $boolStr", a.context)
                  }
                case None =>
                  optNode.entity.asInstanceOf[OptionArg[Boolean]].default.getOrElse(false).unary_!
              }
              Seq(ValueAnchor(optNode.copy(value = Some(boolValue.toString)), a.context))

            //arg def of other type
            case otherTpe =>
              val vOpt = valueOpt match {
                case Some(v) => Some(v)
                case None => a.context.nextArg
              }
              vOpt match {
                case Some(v) => Seq(ValueAnchor(optNode.copy(value = Some(v)), a.context))
                case None =>
                  ArgParseException(
                    s"No value found for opt[$name] with type[${otherTpe.name}].", a.context)
              }
          }

          case None => ArgParseException(s"Unknown option: $name", a.context)
        }
      case bad => ArgParseException(s"Malformed option: $bad", a.context)
    }
  }
}

private object ParamOrCmd extends TypeAbbr with CateUtils {
  implicit val parser: Parser[ParamOrCmd, AnchorEither] = (a: ParamOrCmd) => {
    val cmdNode = a.context.getCurrentCmdNode
    val arg = a.arg

    if (cmdNode.params.nonEmpty) {
      //there's still params to match:
      val paramNode = a.context.nextParamNode
      ???
    } else {
      //there's no params before, a cmd should be matched:
      a.context.nodeAdvance(arg) match{
        case Some(childCmdNode) => ???
        case None => ArgParseException(s"Unknown cmd: $arg", a.context)
      }
    }
  }
}

private sealed trait CateUtils {
  protected implicit
  def seqValue2Right[L](in: Seq[ValueAnchor]): Either[L, Seq[ValueAnchor]] = Right(in)

  /** Parse boolean string literal. */
  protected def parseBoolStr(in: String): Option[Boolean] = in.toLowerCase match {
    case "f" | "false" => Some(false)
    case "t" | "true" => Some(true)
    case _ => None
  }

  protected implicit class ClassTagOps[T](in: ClassTag[T]) {
    def name: String = in.runtimeClass.getSimpleName
  }
}