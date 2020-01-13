name := "NoughtsAndCrossesBot"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.augustjune" %% "canoe" % "0.2.0+9-6a69b5f2+20191102-0327-SNAPSHOT"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

mainClass in assembly := Some("NoughtsAndCrossesBot")
assemblyJarName in assembly := "NoughtsAndCrossesBot.jar"