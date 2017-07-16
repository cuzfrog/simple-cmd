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

    val matchOpt = cmdNode.opts.find(_.entity.abbr.exists(_.head == arg.head))
    matchOpt match {
      case Some(optNode1) => optNode1.tpe.runtimeClass match {
        case rc if rc == classOf[Boolean] => arg match {
          case BooleanLit(bool) => parseBoolLit(bool, a.scope).right.map { bool =>
            Seq(ValueAnchor(optNode1.copy(value = Some(bool)), a.scope))
          }
          case bools if bools.matches("""\w+""") =>
            val boolSet = bools.split("").toSet
            if (boolSet.size < bools.length) {
              ArgParseException(s"Duplicates in boolean options: $bools", a.scope)
            }
            else {
              val boolNodes = cmdNode.opts.collect {
                case node if node.tpe.runtimeClass == classOf[Boolean] => node.asInstanceOf[OptNode[Boolean]]
              }
              boolNodes.filter(n => n.entity.abbr.exists(boolSet.contains))

              boolSet.map { b =>
                boolNodes.find(_.entity.abbr.contains(b)) match {
                  case Some(optNode) =>
                    val boolValue = optNode.entity.default.getOrElse(false).unary_!
                  case None =>
                }
              }


              ???

            }
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
}