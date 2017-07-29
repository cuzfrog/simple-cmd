package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.SingleValue
import com.github.cuzfrog.scmd.internal.SimpleLogging

import scala.annotation.tailrec
import scala.reflect.ClassTag

/*
 * The three categories of args provide low level parsing.
 */

private sealed trait CateArg {
  def arg: String
}
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

      trace(s"parse SingleOpts $arg")

      val matchOpt =
        c.getUpstreamLeftOpts.find(_.entity.abbr.exists(_.head == arg.head)) //match first letter
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
                  ArgParseException(s"Boolean options: -$badArgs not defined", c)
                }
                else if (boolSet.size < bools.length) {
                  ArgParseException(s"Duplicates in boolean options: -$bools", c)
                }
                else {
                  val optNodesWithValue = boolNodes.flatMap { n =>
                    val boolValue = n.entity match {
                      case s: SingleValue[Boolean@unchecked] => s.default.getOrElse(false).unary_!
                      case m =>
                        throw new AssertionError(s"Boolean value cannot be variable." +
                          s" Node:${n.entity.name}")
                    }
                    c.anchors(n.copy(value = Seq(boolValue)))
                  }
                  optNodesWithValue
                }

              case bad =>
                ArgParseException(s"Boolean opts -$bad contain unsupported letter", c)
            }

          //found argDef for other types
          case otherTpe =>
            try {
              val value = arg match {
                case EqualLitertal(v) => v
                case ValueFolding(v) => v
                case single if single.matches("""\w""") => c.nextArg.getOrElse(
                  throw ArgParseException(
                    s"No value found for opt -$arg with type[${otherTpe.name}].", c))
                case bad => throw ArgParseException(s"Malformed opt -$bad", c)
              }
              c.anchors(optNode1.copy(value = Seq(value)))
            } catch {
              case e: ArgParseException => e
            }
        }
        case None =>
          ArgParseException(s"Unknown opt -$arg", c)
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
          val matchOpt = c.getUpstreamLeftOpts.find(_.entity.name == name)
          matchOpt match {
            case Some(optNode) =>
              trace(s"Parse LongOpt $arg -> matched [${optNode.tpe}]")
              optNode.tpe match {
                //arg def of type Boolean
                case ClassTag.Boolean =>
                  valueOpt match {
                    case Some(boolStr) => parseBoolStr(boolStr) match {
                      case Some(b) => c.anchors(optNode.copy(value = Seq(b)))
                      case None => ArgParseException(s"Unknown bool literal: -$arg", c)
                    }
                    case None =>
                      //logic not default boolean value
                      val defaultB = optNode.entity match {
                        case s: SingleValue[Boolean@unchecked] => s.default match {
                          case Some(b: Boolean) => b
                          case _ => false //type boolean checked by ClassTag
                        }
                        case m =>
                          throw new AssertionError(s"Boolean value cannot be variable." +
                            s" Node:${optNode.entity.name}")
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
                        s"No value found for opt -$arg with type[${otherTpe.name}].", c)
                  }
              }

            case None =>
              trace(s"Parse LongOpt $arg -> not-matched.")
              ArgParseException(s"Unknown option: -$arg", c)
          }
        case bad => ArgParseException(s"Malformed option: $bad", c)
      }
    }
  }
}
//todo: convert camel case name to hyphen linked
private object ParamOrCmd extends CateUtils {
  implicit val parser: Parser[ParamOrCmd, AnchorEither] = new Parser[ParamOrCmd, AnchorEither] {
    override def parse(a: ParamOrCmd)
                      (implicit c: Context): AnchorEither = {
      val arg = a.arg

      c.nextParamNode match {
        //there's still params to match:
        case Some(paramNode) =>
          val anchorsWithValue = if (paramNode.isVariable) {
            trace(s"Parse ParamOrCmd:${a.arg} -> param...")
            //variable/multiple args:

            /** Pop args from context and create anchors along the way. */
            @tailrec
            def recFork(acc: Seq[Anchor], values: Seq[String]): Seq[Anchor] = {
              c.nextArgWithType[ParamOrCmd] match {
                case Some(v) =>
                  val accValues = values :+ v
                  debug("ParamNode type before/after evaluation:" +
                    paramNode.tpe + "/" + paramNode.copy(value = accValues).tpe)
                  val newAnchor = c.anchors(paramNode.copy(value = accValues))
                  recFork(acc ++ newAnchor, accValues)
                case None => acc
              }
            }

            val firstAnchor = c.anchors(paramNode.copy(value = Seq(arg)))
            recFork(firstAnchor, Seq(arg)) //current context state should point to last anchors.
          }
          //single arg:
          else {
            trace(s"Parse ParamOrCmd:${a.arg} -> param single")
            c.anchors(paramNode.copy(value = Seq(arg)))
          }

          val possibleCmdAnchor = if (!paramNode.entity.isMandatory) {
            this.consumeCmd(arg, c).right.toSeq.flatten
          } else Seq.empty

          anchorsWithValue ++ possibleCmdAnchor
        //there's no params before, a cmd should be matched:
        case None =>
          trace(s"Parse ParamOrCmd:${a.arg} -> cmd")
          this.consumeCmd(arg, c)
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

private sealed trait CateUtils extends SimpleLogging {
  protected implicit
  def seqValue2Right[L](in: Seq[Anchor]): Either[L, Seq[Anchor]] = Right(in)

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