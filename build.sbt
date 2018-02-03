import sbt.Keys.libraryDependencies
import sbtcrossproject.{CrossType, crossProject}

lazy val taplScala =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("."))
    .settings(
      scalaVersion := "2.12.4",
    )
    .jsSettings(
      libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.0",
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.4",
    )
    .jvmSettings(
      libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    )

lazy val taplScalaJVM = taplScala.jvm
lazy val taplScalaJS = taplScala.js
