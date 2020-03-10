name := "NoughtsAndCrossesBot"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions += "-language:higherKinds"

libraryDependencies += "org.augustjune" %% "canoe" % "0.4.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

mainClass in assembly := Some("NoughtsAndCrossesBot")
assemblyJarName in assembly := "NoughtsAndCrossesBot.jar"