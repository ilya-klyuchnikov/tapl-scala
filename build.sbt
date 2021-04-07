scalaVersion := "3.0.0-RC2"

name := "tapl-scala"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-RC1")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"
