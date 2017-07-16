package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.OptNode


private case class SingleOpts(arg: String,
                              nextArg: Option[String], scope: Scope)
private case class LongOpts(arg: String,
                            nextArg: Option[String], scope: Scope)
private case class ParamOrCmd(arg: String, scope: Scope)

private object SingleOpts {

  private val BooleanLit = """\w=(\w+)""".r

  implicit val parser: Parser[SingleOpts, AnchorEither] = (a: SingleOpts) => {
    val cmdNode = a.scope.cmdNode
    val arg = a.arg

    val matchOpt = cmdNode.opts.find(_.entity.abbr.exists(_.head == arg.head)) //match first letter
    matchOpt match {
      case Some(optNode1) => optNode1.tpe.runtimeClass match {
        case rc if rc == classOf[Boolean] =>
          arg match {
            case BooleanLit(bool) => // arg=literal
              parseBoolLit(bool, a.scope).right.map { bool =>
                Seq(ValueAnchor(optNode1.copy(value = Some(bool)), a.scope))
              }
            case bools if bools.matches("""\w+""") => //folded letters
              val boolSet = bools.split("").toSet
              val boolNodes = cmdNode.opts.collectWithType[OptNode[Boolean]]
                .filter(n => n.entity.abbr.exists(boolSet.contains))
              if (boolNodes.size < boolSet.size) {
                val badArgs =
                  boolSet.filterNot(s => boolNodes.flatMap(_.entity.abbr).contains(s)).mkString
                ArgParseException(s"Boolean options:[$badArgs] not defined.", a.scope)
              }
              else if (boolSet.size < bools.length) {
                ArgParseException(s"Duplicates in boolean options: $bools", a.scope)
              }
              else {
                val optNodesWithValue = boolNodes.map { n =>
                  val boolValue = n.entity.default.getOrElse(false).unary_!
                  ValueAnchor(n.copy(value = Option(boolValue)), a.scope)
                }
                optNodesWithValue
              }

            case bad =>
              ArgParseException(s"Boolean opts[$bad] contain unsupported letter.", a.scope)
          }

        case _ =>
      }
      case None =>
        ArgParseException(s"Unknow opt[$arg]", a.scope)
    }
  }

  private def parseBoolLit(bool: String, scope: Scope): Either[ArgParseException, Boolean] = {
    bool.toLowerCase match {
      case "f" | "false" => Right(false)
      case "t" | "true" => Right(true)
      case bad => ArgParseException(s"Unknow bool literal: $bad", scope)
    }
  }

  private implicit def seqValue2Right[L](in: Seq[ValueAnchor]): Either[L, Seq[ValueAnchor]] = Right(in)
}