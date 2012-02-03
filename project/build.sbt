resolvers ++= Seq(Classpaths.typesafeResolver, "sbt-idea-repo" at "http://mpeltonen.github.com/maven/")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0-M3")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")