name := """cards"""
organization := "io.github.jmisabella"

version := "0.1"

scalaVersion := "2.13.6"

publishTo := sonatypePublishTo.value

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.9.3",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.13.4",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)
