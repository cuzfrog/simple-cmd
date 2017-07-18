package com.github.cuzfrog.scmd.parse

import scala.annotation.tailrec
import scala.reflect.ClassTag

/*
 * The three categories of args provide low level parsing.
 */

private sealed trait CateArg {def arg: String}
private object CateArg {
  implicit val parser: Parser[CateArg, AnchorEither] = new Parser[CateArg, AnchorEither] {
    override def parse(a: CateArg)(implicit context: Context): AnchorEither = a match {
      case s: SingleOpts => s.parsed
      case l: LongOpt => l.parsed
      case pm: ParamOrCmd => pm.parsed
    }
  }
}

/** Opt(s) with single letter. "-" has been stripped off. */
private case class SingleOpts(arg: String) extends CateArg
/** Opt with full name. One "-" of the "--" has been stripped off. */
private case class LongOpt(arg: String) extends CateArg
/** Param or Cmd with no prefix "-". */
private case class ParamOrCmd(arg: String) extends CateArg


private object SingleOpts extends CateUtils {
  private val EqualLitertal = """\w=(\w+)""".r
  private val ValueFolding = """\w([^\=]{1}.*)""".r

  implicit val parser: Parser[SingleOpts, AnchorEither] = new Parser[SingleOpts, AnchorEither] {
    override def parse(a: SingleOpts)(implicit c: Context): AnchorEither = {
      val cmdNode = c.getCurrentCmdNode
      val arg = a.arg

      val matchOpt =
        cmdNode.opts.find(_.entity.abbr.exists(_.head == arg.head)) //match first letter
      matchOpt match {
        case Some(optNode1) => optNode1.tpe match {
          //found argDef for type Boolean
          case ClassTag.Boolean =>
            arg match {
              //arg=literal
              case EqualLitertal(bool) =>
                parseBoolStr(bool) match {
                  case Some(b) => c.anchors(optNode1.copy(value = Seq(b)))
                  case None => ArgParseException(s"Unknown bool literal: $bool", c)
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
                  ArgParseException(s"Boolean options:[$badArgs] not defined", c)
                }
                else if (boolSet.size < bools.length) {
                  ArgParseException(s"Duplicates in boolean options: $bools", c)
                }
                else {
                  val optNodesWithValue = boolNodes.flatMap { n =>
                    val boolValue = n.entity.default.getOrElse(false).unary_!
                    c.anchors(n.copy(value = Seq(boolValue)))
                  }
                  optNodesWithValue
                }

              case bad =>
                ArgParseException(s"Boolean opts[$bad] contain unsupported letter", c)
            }

          //found argDef for other types
          case otherTpe =>
            try {
              val value = arg match {
                case EqualLitertal(v) => v
                case ValueFolding(v) => v
                case single if single.matches("""\w""") => c.nextArg.getOrElse(
                  throw ArgParseException(
                    s"No value found for opt[$arg] with type[${otherTpe.name}].", c))
                case bad => throw ArgParseException(s"Malformed opt[$bad]", c)
              }
              c.anchors(optNode1.copy(value = Seq(value)))
            } catch {
              case e: ArgParseException => e
            }
        }
        case None =>
          ArgParseException(s"Unknown opt[$arg]", c)
      }
    }
  }
}

private object LongOpt extends CateUtils {
  private val EqualLiteral = """-([\-\w\d]+)(=.*)?""".r

  implicit val parser: Parser[LongOpt, AnchorEither] = new Parser[LongOpt, AnchorEither] {
    override def parse(a: LongOpt)(implicit c: Context): AnchorEither = {
      val cmdNode = c.getCurrentCmdNode
      val arg = a.arg

      arg match {
        case EqualLiteral(name, e_Value) =>
          val valueOpt = Option(e_Value).map(_.drop(1))
          val matchOpt = cmdNode.opts.find(_.entity.name == name)
          matchOpt match {
            case Some(optNode) => optNode.tpe match {
              //arg def of type Boolean
              case ClassTag.Boolean =>
                valueOpt match {
                  case Some(boolStr) => parseBoolStr(boolStr) match {
                    case Some(b) => c.anchors(optNode.copy(value = Seq(b)))
                    case None => ArgParseException(s"Unknown bool literal: $arg", c)
                  }
                  case None =>
                    //logic not default boolean value
                    val defaultB = optNode.entity.default match {
                      case Some(b: Boolean) => b
                      case _ => false //type boolean checked by ClassTag
                    }
                    c.anchors(optNode.copy(value = Seq(defaultB.unary_!)))
                }

              //arg def of other type
              case otherTpe =>
                val vOpt = valueOpt match {
                  case Some(v) => Some(v)
                  case None => c.nextArg
                }
                vOpt match {
                  case Some(v) => c.anchors(optNode.copy(value = Seq(v)))
                  case None =>
                    ArgParseException(
                      s"No value found for opt[$name] with type[${otherTpe.name}].", c)
                }
            }

            case None => ArgParseException(s"Unknown option: $name", c)
          }
        case bad => ArgParseException(s"Malformed option: $bad", c)
      }
    }
  }
}

private object ParamOrCmd extends CateUtils {
  implicit val parser: Parser[ParamOrCmd, AnchorEither] = new Parser[ParamOrCmd, AnchorEither] {
    override def parse(a: ParamOrCmd)
                      (implicit c: Context): AnchorEither = {
      val arg = a.arg

      c.nextParamNode match {
        //there's still params to match:
        case Some(paramNode) =>
          val anchorsWithValue = paramNode.tpe.runtimeClass match {
            //variable/multiple args:
            case rc if rc == classOf[Seq[_]] || rc == classOf[List[_]] =>

              /** Pop args from context and create anchors along the way. */
              @tailrec
              def recFork(acc: Seq[Anchor[_]], values: Seq[String]): Seq[Anchor[_]] = {
                c.nextArgWithType[ParamOrCmd] match {
                  case Some(v) =>
                    val accValues = values :+ v
                    val newAnchor = c.anchors(paramNode.copy(value = accValues))
                    recFork(acc ++ newAnchor, accValues)
                  case None => acc
                }
              }

              val firstAnchor = c.anchors(paramNode.copy(value = Seq(arg)))
              recFork(firstAnchor, Seq(arg)) //current context state should point to last anchors.

            //single arg:
            case _ =>
              c.anchors(paramNode.copy(value = Seq(arg)))
          }

          val possibleCmdAnchor = if (!paramNode.entity.isMandatory) {
            this.consumeCmd(arg, c).right.toSeq.flatten
          } else Seq.empty

          anchorsWithValue ++ possibleCmdAnchor
        //there's no params before, a cmd should be matched:
        case None => this.consumeCmd(arg, c)
      }
    }

    /** Consume an arg as a cmd. */
    private def consumeCmd(arg: String, c: Context): AnchorEither = {
      c.nodeAdvance(arg) match {
        case Some(childCmdNode) => c.anchors(childCmdNode)
        case None => ArgParseException(s"Unknown cmd: $arg", c)
      }
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

  protected implicit def boolean2string(b: Boolean): String = b.toString
}