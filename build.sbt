name := "zeroone"

version := "1.0"

scalaVersion := "2.12.1"
scalacOptions += "-feature"

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

initialCommands in console := "import zeroone._, ZeroOne._, StackEffect._"