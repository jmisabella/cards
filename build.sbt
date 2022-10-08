name := """cards"""
organization := "com.jmisabella"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.9.3",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.13.4",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)

