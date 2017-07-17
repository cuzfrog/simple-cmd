package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.OptNode

/** Opt(s) with single letter. "-" has been stripped off. */
private case class SingleOpts(arg: String, context: Context)
/** Opt with full name. One "-" of the "--" has been stripped off. */
private case class LongOpt(arg: String, context: Context)
/** Param or Cmd with no prefix "-". */
private case class ParamOrCmd(arg: String, context: Context)

private object SingleOpts {

  private val EqualLitertal = """\w=(\w+)""".r
  private val ValueFolding = """\w([^\=]{1}.*)""".r

  implicit val parser: Parser[SingleOpts, AnchorEither] = (a: SingleOpts) => {
    val cmdNode = a.context.getCurrentNode
    val arg = a.arg

    val matchOpt = cmdNode.opts.find(_.entity.abbr.exists(_.head == arg.head)) //match first letter
    matchOpt match {
      case Some(optNode1) => optNode1.tpe.runtimeClass match {
        //found argDef for type Boolean
        case rc if rc == classOf[Boolean] =>
          arg match {
            //arg=literal
            case EqualLitertal(bool) =>
              parseBoolLit(bool, a.context).right.map { bool =>
                Seq(ValueAnchor(optNode1.copy(value = Some(bool)), a.context))
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
        case rc =>
          try {
            val value = arg match {
              case EqualLitertal(v) => v
              case ValueFolding(v) => v
              case single if single.matches("""\w""") => a.context.nextArg.getOrElse(
                throw ArgParseException(
                  s"No value found for opt with type[${rc.getSimpleName}].", a.context)
              )
              case bad => throw ArgParseException(s"Malformed opt[$bad]", a.context)
            }
            Seq(ValueAnchor(optNode1.copy(value = Some(value)), a.context))
          } catch {
            case e: ArgParseException => e
          }
      }
      case None =>
        ArgParseException(s"Unknow opt[$arg]", a.context)
    }
  }

  private def parseBoolLit(bool: String, scope: Context): Either[ArgParseException, String] = {
    bool.toLowerCase match {
      case "f" | "false" => Right(false.toString)
      case "t" | "true" => Right(true.toString)
      case bad => ArgParseException(s"Unknow bool literal: $bad", scope)
    }
  }

  private implicit def seqValue2Right[L](in: Seq[ValueAnchor]): Either[L, Seq[ValueAnchor]] = Right(in)
}

private object LongOpt {
  private val EqualLiteral = """-([\-\w\d]+)(=.*)?""".r

  implicit val parser: Parser[LongOpt, AnchorEither] = (a: LongOpt) => {
    val cmdNode = a.context.getCurrentNode
    val arg = a.arg
    arg match {
      case EqualLiteral(name, e_Value) => 
        val valueOpt = Option(e_Value).map(_.drop(1))
        val matchOpt = cmdNode.opts.find(_.entity.name == name)
        matchOpt match{
          case Some(optNode) =>
          case None => ArgParseException(s"Unknow option: $name", a.context)
        }
      case bad => ArgParseException(s"Malformed option: $bad", a.context)
    }
  }
}