import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType

lazy val taplScala =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("."))
    .settings(
      scalaVersion := "2.12.6",
    )
    .jvmSettings(
      libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
      libraryDependencies += "io.circe" %% "circe-core" % "0.9.3",
      libraryDependencies += "io.circe" %% "circe-generic" % "0.9.3",
      libraryDependencies += "io.circe" %% "circe-parser" % "0.9.3",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    )
    .jsSettings(
      libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.1",
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6",
    )

lazy val taplScalaJVM = taplScala.jvm
lazy val taplScalaJS = taplScala.js
