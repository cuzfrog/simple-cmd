import CommonSettings._

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }
//onLoad in Global := (onLoad in Global).value andThen (Command.process(s"", _))
scalaVersion in ThisBuild := "2.11.11"
//logLevel := Level.Debug

val macroAnnotationSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in(Compile, console) ~= (_ filterNot (_ contains "paradise")),
  //scalacOptions in(Compile, doc) ~= (_ filterNot (_ contains "paradise")),
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
  )

val scmd = (project in file("."))
  .settings(
    commonSettings, publishSettings, macroAnnotationSettings
  )
  .settings(
    name := "scmd",
    description := "simple-cmd, a command-line argument parsing library for scala.",
    mappings in(Compile, packageBin) ++= mappings.in(internalMacros, Compile, packageBin).value,
    mappings in(Compile, packageSrc) ++= mappings.in(internalMacros, Compile, packageSrc).value,
    doc in Compile := {new File("/dev/null")},
    scalastyleConfig := baseDirectory.value / "project" / "scalastyle-config.xml"
    //scalacOptions in(Compile, doc) ++= Seq("-Ydoc-debug", "-verbose")
  ).dependsOn(internalMacros % "compile-internal, test-internal")


val tests: Project = project
  .settings(commonSettings, macroAnnotationSettings)
  .settings(
    name := "simple-cmd-tests",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
    ),
    test in Test := test.in(Test).triggeredBy(test.in(Test).in(scmd)).value
  ).dependsOn(scmd % "compile->test;test->test", internalMacros % "compile-internal, test-internal")

import ReleaseTransformations._

pgpReadOnly := false
pgpSecretRing := baseDirectory.value / "project" / "codesigning.asc"
pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray)
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  releaseStepCommand("pgp-cmd recv-key 895B79DB hkp://keyserver.ubuntu.com"),
  releaseStepCommand("+publishSigned"),
  releaseStepCommand("sonatypeReleaseAll")
)

val web = project
  .enablePlugins(HugoPlugin)