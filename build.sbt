name := "Simplify Polygon"

version := "1.0"

organization := "net.hasnext.simplifyPolygon"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

libraryDependencies += "net.liftweb" % "lift-json_2.9.0" % "2.4-M3"

seq(sbtassembly.Plugin.assemblySettings: _*)

  mainClass := Some("net.hasnext.mapping.ProcessFile")

  scalaVersion := "2.9.0"

  scalacOptions += "-deprecation"

  parallelExecution := true

  name := "SimplifyPolygon"

  version := "1.0"

  scalaVersion := "2.9.0-1"

  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

  libraryDependencies ++= Seq(
      "se.scalablesolutions.akka" % "akka-actor" % "1.1.3",
      "se.scalablesolutions.akka" % "akka-typed-actor" % "1.1.3",
      "se.scalablesolutions.akka" % "akka-amqp" % "1.1.3",
      "se.scalablesolutions.akka" % "akka-testkit" % "1.1.3"
      )

