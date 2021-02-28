scalaVersion := "3.0.0-RC1"

name := "tapl-scala"

scalacOptions ++= Seq("-deprecation", "-feature", "-source", "3.0-migration")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"
