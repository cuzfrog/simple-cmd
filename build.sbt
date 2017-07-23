import CommonSettings._

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }
onLoad in Global := (onLoad in Global).value andThen (Command.process(s"", _))
scalaVersion in ThisBuild := "2.12.2"
val CompileOnly = config("compileonly").hide

val macroAnnotationSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
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

val shared = project
  .settings(commonSettings, macroAnnotationSettings)
  .settings(
    name := "simple-cmd-shared"
  ).dependsOn(internalMacros)

val macros = project
  .settings(commonSettings, macroAnnotationSettings)
  .settings(
    name := "simple-cmd-macros",
    libraryDependencies ++= {
      val monocleVersion = "1.4.0"
      Seq(
        "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
        "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
        "com.github.julien-truffaut" %% "monocle-law" % monocleVersion % "test",
        "org.scalaz" %% "scalaz-core" % "7.2.14",
        "org.scalameta" %% "contrib" % "1.8.0"
      )
    },
    libraryDependencies ++= loggingDependencies
  ).dependsOn(shared)

val runtime = project
  .settings(commonSettings, macroAnnotationSettings)
  .settings(
    name := "simple-cmd-runtime"
  ).dependsOn(shared, macros % Provided)

val tests = project
  .settings(commonSettings, macroAnnotationSettings)
  .settings(
    name := "simple-cmd-tests",
    libraryDependencies ++= Seq(

    )
  ).dependsOn(runtime % "compile->test;test->test", macros % Provided)

val bundle = (project in file("."))
  .settings(
    commonSettings, publicationSettings, readmeVersionSettings, macroAnnotationSettings
  )
  .settings(
    name := "simple-cmd"
  ).dependsOn(runtime, macros)