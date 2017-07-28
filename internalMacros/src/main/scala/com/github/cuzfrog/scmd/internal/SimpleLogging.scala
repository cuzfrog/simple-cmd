package com.github.cuzfrog.scmd.internal

import scala.util.Try

/**
  * Override val loggerAgent for logger name display.
  * Override val loggerLevel for manual level control.
  *
  * Created by cuz on 2016-08-17.
  */
private[scmd] trait SimpleLogging {

  import SimpleLogging._
  import Console._

  private val LIGHT_BLUE = "\u001b[94m"

  protected implicit val loggerAgent: LoggerAgent = this.getClass.getName
  protected implicit val loggerLevel: Level = queryLevel(this.getClass.getName)

  protected def trace(x: => Any, withTitle: Boolean = true): Unit = pl(x, Trace, LIGHT_BLUE)(withTitle)
  protected def debug(x: => Any, withTitle: Boolean = true): Unit = pl(x, Debug, MAGENTA)(withTitle)
  protected def info(x: => Any, withTitle: Boolean = true): Unit = pl(x, Info, GREEN)(withTitle)
  protected def warn(x: => Any, withTitle: Boolean = true): Unit = pl(x, Warn, YELLOW)(withTitle)
  protected def err(x: => Any, withTitle: Boolean = true): Unit = pl(x, Error, RED)(withTitle)

  private def pl(x: => Any, level: Level, color: String = "")
                (withTitle: Boolean)
                (implicit agent: LoggerAgent, levelThreshold: Level) = {
    lazy val prefix = if (withTitle) s"[$agent][$color$level$RESET]" else ""
    if (level.level >= levelThreshold.level) println(s"$prefix$x")
  }


}

private[scmd] object SimpleLogging {
  class LoggerAgent(val name: String) extends AnyVal{
    override def toString: String = name
  }
  object LoggerAgent {
    implicit def fromString(name: String): LoggerAgent = new LoggerAgent(name)
  }

  sealed trait Level {val level: Int}
  object Level {
    def fromString(in: String): Level = in.toLowerCase match {
      case "trace" => Trace
      case "debug" => Debug
      case "info" => Info
      case "warn" => Warn
      case "error" => Error
      case bad => throw new IllegalArgumentException(s"Unknown log level $bad")
    }
  }
  case object Trace extends Level {val level = -2}
  case object Debug extends Level {val level = -1}
  case object Info extends Level {val level = 0}
  case object Warn extends Level {val level = 1}
  case object Error extends Level {val level = 2}

  private val properties: Map[String, Level] = {
    val x = new java.util.Properties
    val result = Try(
      x.load(scala.io.Source.fromResource("simple-logger.properties").reader())
    )
    import scala.collection.JavaConverters._
    if (result.isFailure) Map.empty
    else x.asScala.map { case (k, v) => k -> Level.fromString(v) }.toMap
  }

  private def queryLevel(path: String): Level = {
    val exact = properties.find { case (k, v) => k == path }
    exact match {
      case Some((k, l)) => l
      case None => properties.find { case (k, v) => path.startsWith(k) } match {
        case Some((k, l)) => l
        case None => Info //default level info
      }
    }
  }
}

