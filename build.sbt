import sbt.Keys._
import Build._

val buildNumber = sys.env.get("BUILD_NUMBER").getOrElse("SNAPSHOT")

lazy val commonSettings = Seq(
  scalaVersion in ThisBuild := "2.12.1",
  name := """rpnkata""",
  fork in run := true,
  organization := "io.jorand",
  version := s"0.1-$buildNumber"

)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      cats, shapeless, scalactic, scalatest, scalalogging, logback
    )
  )