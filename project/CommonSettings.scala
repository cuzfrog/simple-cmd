import java.nio.file.Paths

import sbt.Keys._
import sbt._
import MyTasks._

object CommonSettings {
  val commonSettings = Seq(
    organization := "com.github.cuzfrog",
    crossScalaVersions := Seq("2.11.11", "2.12.3"),
    scalacOptions ++= Seq(
      //"-Xlint",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials"),
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.12" % "test",
      "com.novocode" % "junit-interface" % "0.11" % "test->default"
    ),
    //autoAPIMappings := true,
    logBuffered in Test := false,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-q", "-a"),
    parallelExecution in Test := false
  )

  val publishSettings = Seq(
    publishMavenStyle := true,
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/cuzfrog/simple-cmd"),
        "scm:git@github.com:cuzfrog/simple-cmd.git"
      )
    ),
    developers := List(
      Developer(id = "cuzfrog", name = "Cause Chung",
        email = "cuzfrog@139.com", url = url("https://github.com/cuzfrog/"))
    ),
    licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
    homepage := Some(url("https://github.com/cuzfrog/simple-cmd"))
  )
}