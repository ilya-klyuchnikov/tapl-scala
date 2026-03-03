scalaVersion := "3.3.7"

name := "tapl-scala"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
