enablePlugins(Antlr4Plugin)

scalaVersion := "2.13.4"

name := "tapl-scala"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

antlr4Version in Antlr4 := "4.9.1"

antlr4PackageName in Antlr4 := Some("tapl.grammars")

// not great (it removes src/main/java from java source),
// but working (generated grammars are put into java source automatically)
// solution for configurations
javaSource in Compile := (javaSource in Antlr4).value
