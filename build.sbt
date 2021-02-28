scalaVersion := "3.0.0-RC1"

name := "tapl-scala"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2").withDottyCompat(scalaVersion.value)
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"
