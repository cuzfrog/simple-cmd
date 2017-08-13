import CommonSettings._

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }
//onLoad in Global := (onLoad in Global).value andThen (Command.process(s"", _))
scalaVersion in ThisBuild := "2.12.3"
val CompileOnly = config("compileonly").hide

val macroAnnotationSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in(Compile, console) ~= (_ filterNot (_ contains "paradise")),
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0" % Provided
)

val internalMacros = project
  .settings(commonSettings, macroAnnotationSettings)
  .settings(
    name := "simple-cmd-internal",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    ),
    publish := {},
    publishLocal := {}
  ).configs(CompileOnly)

val scmd = (project in file("."))
  .settings(
    commonSettings, publicationSettings, readmeVersionSettings, macroAnnotationSettings
  )
  .settings(
    name := "simple-cmd"
  ).dependsOn(internalMacros)

val tests = project
  .settings(commonSettings, macroAnnotationSettings)
  .settings(
    name := "simple-cmd-tests",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
    )
  ).dependsOn(scmd % "compile->test;test->test")

