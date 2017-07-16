package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.OptNode


private case class SingleOpts(arg: String,
                              nextArg: Option[String], scope: Scope)
private case class LongOpts(arg: String,
                            nextArg: Option[String], scope: Scope)
private case class ParamOrCmd(arg: String, scope: Scope)

private object SingleOpts {

  private val EqualLitertal = """\w=(\w+)""".r
  private val ValueFolding = """\w([^\=]{1}.*)""".r

  implicit val parser: Parser[SingleOpts, AnchorEither] = (a: SingleOpts) => {
    val cmdNode = a.scope.cmdNode
    val arg = a.arg

    val matchOpt = cmdNode.opts.find(_.entity.abbr.exists(_.head == arg.head)) //match first letter
    matchOpt match {
      case Some(optNode1) => optNode1.tpe.runtimeClass match {
        //found argDef for type Boolean
        case rc if rc == classOf[Boolean] =>
          arg match {
            //arg=literal
            case EqualLitertal(bool) =>
              parseBoolLit(bool, a.scope).right.map { bool =>
                Seq(ValueAnchor(optNode1.copy(value = Some(bool)), a.scope))
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
                ArgParseException(s"Boolean options:[$badArgs] not defined", a.scope)
              }
              else if (boolSet.size < bools.length) {
                ArgParseException(s"Duplicates in boolean options: $bools", a.scope)
              }
              else {
                val optNodesWithValue = boolNodes.map { n =>
                  val boolValue = n.entity.default.getOrElse(false).unary_!.toString
                  ValueAnchor(n.copy(value = Some(boolValue)), a.scope)
                }
                optNodesWithValue
              }

            case bad =>
              ArgParseException(s"Boolean opts[$bad] contain unsupported letter", a.scope)
          }

        //found argDef for other types
        case rc =>
          try {
            val value = arg match {
              case EqualLitertal(v) => v
              case ValueFolding(v) => v
              case single if single.matches("""\w""") => a.nextArg.getOrElse(
                throw ArgParseException(
                  s"No value found for opt with type[${rc.getSimpleName}].", a.scope)
              )
              case bad => throw ArgParseException(s"Malformed opt[$bad]", a.scope)
            }
            Seq(ValueAnchor(optNode1.copy(value = Some(value)), a.scope))
          } catch {
            case e: ArgParseException => e
          }
      }
      case None =>
        ArgParseException(s"Unknow opt[$arg]", a.scope)
    }
  }

  private def parseBoolLit(bool: String, scope: Scope): Either[ArgParseException, String] = {
    bool.toLowerCase match {
      case "f" | "false" => Right(false.toString)
      case "t" | "true" => Right(true.toString)
      case bad => ArgParseException(s"Unknow bool literal: $bad", scope)
    }
  }

  private implicit def seqValue2Right[L](in: Seq[ValueAnchor]): Either[L, Seq[ValueAnchor]] = Right(in)
}