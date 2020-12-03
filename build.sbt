ThisBuild / organization := "org.data.structures.and.algorithms"

ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "2.13.4"

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:implicitConversions")

ThisBuild / fork := true

lazy val root = Project("data-structures-and-algorithms", file("."))
