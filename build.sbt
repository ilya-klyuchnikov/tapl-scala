enablePlugins(ScalaJSPlugin)

scalaVersion := "2.12.3"

name := "tapl-scala"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.3"
