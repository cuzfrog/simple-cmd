package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.ScmdUtils.CanFormPrettyString
import com.github.cuzfrog.scmd.internal.SimpleLogging
import ScmdUtils._

import scala.reflect.ClassTag

sealed trait ArgRoute extends SimpleLogging {
  /** Trigger the route to run. */
  protected def execute: Boolean

  private[scmd] def queryPrior(cmdSymbol: scala.Symbol): Seq[PriorArg]
}

private sealed case class CmdRoute(cmd: Command,
                                   conditions: Seq[RouteCondition] = Nil,
                                   priorActions: Seq[(PriorArg, () => Unit)],
                                   next: Option[ArgRoute] = None) extends ArgRoute {
  /**
    * Add inner func, not really run or execute.
    *
    * @param innerF code to run later, could be an inner route.
    * @tparam R the return type of inner statement.
    * @return an ArgRoute that encapsulate inner route or inner func.
    */
  def run[R: ClassTag](innerF: => R): ArgRoute = {
    val tpe = implicitly[ClassTag[R]]
    val _next: Option[ArgRoute] = tpe.runtimeClass match {
      case rc if rc == classOf[ArgRoute] => Option(RunRoute(() => innerF, isInnerRoute = true))
      case _ => Option(RunRoute(() => innerF, isInnerRoute = false))
    }
    this.copy(next = _next)
  }

  /** Trigger the run func. */
  override def execute: Boolean = this.cmd.met && {
    debug(s"cmdRoute executed: ${this.asInstanceOf[CmdRoute].prettyString}")
    val actions = priorActions collect {
      case (prior, action) if prior.met.contains(cmd.symbol) => action()
    }
    if (actions.isEmpty)
      conditions.forall(_.condition == true) && next.forall(_.execute)
    else true
  }
  override def queryPrior(cmdSymbol: Symbol): Seq[PriorArg] = {
    this.next.map(_.queryPrior(cmdSymbol)).toSeq.flatten ++
      (if (this.cmd.symbol == cmdSymbol) this.priorActions.map(_._1) else Nil)
  }

  def filterPriorActions(f: PriorArg => Boolean): CmdRoute = {
    val filtered = this.priorActions.filter { pa => f(pa._1) }
    this.copy(priorActions = filtered)
  }
}

//private final class LazyRoute(inner: => ArgRoute) extends ArgRoute {
//  override protected def execute = ???
//  override private[scmd] def queryPrior(cmdSymbol: Symbol) = Nil
//}

private final case class RunRoute(runF: (() => R) forSome {type R},
                                  isInnerRoute: Boolean) extends ArgRoute {
  override def execute: Boolean = if (isInnerRoute) {
    debug(s"innerRoute executed:~")
    runF().asInstanceOf[ArgRoute].execute
  } else {
    debug(s"runRoute executed:${this.asInstanceOf[RunRoute].prettyString}")
    runF()
    true
  }
  override def queryPrior(cmdSymbol: Symbol): Seq[PriorArg] = Nil
}
private final case class LinkRoute(seq: Seq[ArgRoute], exhaustive: Boolean) extends ArgRoute {
  override def execute: Boolean = {
    debug(s"linkRoute executed: ${this.asInstanceOf[ArgRoute].prettyString}")
    if (exhaustive) seq.map(_.execute).exists(identity)
    else seq.exists(_.execute)
  }
  override def queryPrior(cmdSymbol: Symbol): Seq[PriorArg] = {
    this.seq.flatMap(_.queryPrior(cmdSymbol))
  }
}

object ArgRoute {
  /** Method execute need to be accessed publicly, yet hided during building by DSL. */
  implicit class executionHelper(in: ArgRoute) {
    def execute: Boolean = in.execute
  }

  /**
    * Merge two routes by filter the left route's priorArgs that are not in the right route.
    * PriorArgs are compared with the same scope of command.<br>
    * <br>
    * This method is public for macro, not a client api.
    */
  def mergePriors(to: ArgRoute, from: ArgRoute): ArgRoute = {
    import ScmdRouteDSL._
    to match {
      case cru: CmdRoute =>
        val filtered = cru.filterPriorActions(p => !from.queryPrior(cru.cmd.symbol).contains(p))
        filtered & from
      case _ => throw new UnsupportedOperationException("Only cmd route can be merged to.")
    }
  }

  private[scmd] implicit def canFormPrettyString: CanFormPrettyString[ArgRoute] =
    new CanFormPrettyString[ArgRoute] {
      override def mkPrettyString(a: ArgRoute): String = recPrettyString(a)
    }
  private def recPrettyString(r: ArgRoute, indent: String = " "): String = r match {
    case r: CmdRoute => indent + r.prettyString
    case r: LinkRoute => s"LinkRoute(exhaustive:${r.exhaustive}):" +
      NEWLINE + r.seq.map(lr => recPrettyString(lr, indent + " ")).mkString(NEWLINE)
    case r: RunRoute => indent + r.prettyString
  }
}

private object CmdRoute {
  implicit def canFormPrettyString: CanFormPrettyString[CmdRoute] =
    new CanFormPrettyString[CmdRoute] {
      override def mkPrettyString(a: CmdRoute): String = {
        val next = a.next match {
          case None => None
          case Some(r) => r.prettyString
        }
        s"CmdRoute(${a.cmd.name}->$next)"
      }
    }
}

private object RunRoute {
  implicit def canFormPrettyString: CanFormPrettyString[RunRoute] =
    new CanFormPrettyString[RunRoute] {
      override def mkPrettyString(a: RunRoute): String = {
        s"RunRoute"
      }
    }
}