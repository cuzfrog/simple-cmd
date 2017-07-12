import CommonSettings._

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }
onLoad in Global := (onLoad in Global).value andThen (Command.process(s"", _))

val macroAnnotationSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise"))
)

val root = (project in file("."))
  .settings(
    commonSettings, publicationSettings, readmeVersionSettings, macroAnnotationSettings
  )
  .settings(
    name := "simple-cmd",
    libraryDependencies ++= Seq(

    )
  )

val macros = (project in file("./macros"))
  .settings(commonSettings, macroAnnotationSettings)
  .settings(
    name := "simple-cmd-macros",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "1.8.0" % "compileonly"
    )
  )
