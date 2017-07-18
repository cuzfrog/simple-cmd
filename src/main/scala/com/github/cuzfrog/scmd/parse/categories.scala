package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.OptionArg

import scala.annotation.tailrec
import scala.reflect.ClassTag

/*
 * The three categories of args provide low level parsing.
 */

private sealed trait ArgCate
/** Opt(s) with single letter. "-" has been stripped off. */
private case class SingleOpts(arg: String, context: Context) extends ArgCate
/** Opt with full name. One "-" of the "--" has been stripped off. */
private case class LongOpt(arg: String, context: Context) extends ArgCate
/** Param or Cmd with no prefix "-". */
private case class ParamOrCmd(arg: String, context: Context) extends ArgCate


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
                case Some(b) => Seq(Anchor(optNode1.copy(value = Seq(b.toString)), a.context))
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
                  Anchor(n.copy(value = Seq(boolValue)), a.context)
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
            Seq(Anchor(optNode1.copy(value = Seq(value)), a.context))
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
              Seq(Anchor(optNode.copy(value = Seq(boolValue.toString)), a.context))

            //arg def of other type
            case otherTpe =>
              val vOpt = valueOpt match {
                case Some(v) => Some(v)
                case None => a.context.nextArg
              }
              vOpt match {
                case Some(v) => Seq(Anchor(optNode.copy(value = Seq(v)), a.context))
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

    a.context.nextParamNode match {
      //there's still params to match:
      case Some(paramNode) =>
        val anchorsWithValue = paramNode.tpe.runtimeClass match {
          //variable/multiple args:
          case rc if rc == classOf[Seq[_]] || rc == classOf[List[_]] =>

            /** Pop args from context and create anchors along the way. */
            @tailrec
            def recFork(acc: Seq[Anchor[_]], values: Seq[String]): Seq[Anchor[_]] = {
              a.context.nextArgWithType[ParamOrCmd] match {
                case Some(v) =>
                  val accValues = values :+ v
                  val newAnchor = Anchor(paramNode.copy(value = accValues), a.context)
                  recFork(acc :+ newAnchor, accValues)
                case None => acc
              }
            }

            val firstAnchor = Anchor(paramNode.copy(value = Seq(arg)), a.context)
            recFork(Seq(firstAnchor), Seq(arg))

          //single arg:
          case _ =>
            Seq(Anchor(paramNode.copy(value = Seq(arg)), a.context))
        }

        val possibleCmdAnchor = if (!paramNode.entity.isMandatory) {
          this.consumeCmd(arg, a.context).right.toSeq.flatten
        } else Seq.empty

        anchorsWithValue ++ possibleCmdAnchor
      //there's no params before, a cmd should be matched:
      case None => this.consumeCmd(arg, a.context)
    }
  }

  /** Consume an arg as a cmd. */
  private def consumeCmd(arg: String, context: Context): AnchorEither = {
    context.nodeAdvance(arg) match {
      case Some(childCmdNode) => Seq(Anchor(childCmdNode, context))
      case None => ArgParseException(s"Unknown cmd: $arg", context)
    }
  }
}

private sealed trait CateUtils {
  protected implicit
  def seqValue2Right[L](in: Seq[Anchor[_]]): Either[L, Seq[Anchor[_]]] = Right(in)

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