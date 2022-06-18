name := """cards"""
organization := "com.jmisabella"

version := "0.1"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test,
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  filters
)

