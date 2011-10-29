name := "Simplify Polygon"

version := "1.0"

organization := "net.hasnext.simplifyPolygon"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

libraryDependencies += "net.liftweb" % "lift-json_2.9.0" % "2.4-M3"

  mainClass := Some("net.hasnext.mapping.ProcessFile")

  scalaVersion := "2.9.1"

  scalacOptions += "-deprecation"

  parallelExecution := true
