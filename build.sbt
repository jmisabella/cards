
ThisBuild / name := "cards"
ThisBuild / organization := "io.github.jmisabella"
ThisBuild / organizationName := "jmisabella"
ThisBuild / organizationHomepage := Some(url("https://github.com/jmisabella"))
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "2.13.6"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/jmisabella/cards"),
    "scm:git@github.com:jmisabella/cards.git"
  )  
)
ThisBuild / developers := List(
  Developer(
    id = "jmisabella",
    name = "Jeffrey Isabella",
    email = "jeff.isabella@gmail.com",
    url = url("https://github.com/jmisabella")
  )
)

ThisBuild / libraryDependencies ++= Seq(
  // "com.typesafe.play" %% "play-json" % "2.9.3",
  "com.typesafe.play" %% "play-json" % "2.7.4",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.13.4",
  "com.google.code.gson" % "gson" % "2.9.1",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)
ThisBuild / description := "Scala library for card-playing functionality, including games Blackjack and Thirty-One"
ThisBuild / licenses := List(
  "Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")
)
ThisBuild / homepage := Some(url("https://github.com/jmisabella/cards"))
// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

//publishTo := sonatypePublishTo.value
