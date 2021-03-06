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

lazy val root = Project("data-structures-and-algorithms", file(".")).aggregate(
  foundation,
  fundamental,
  arrays
)

lazy val foundation = project.in(file("foundation"))

lazy val fundamental = project.in(file("fundamental"))

lazy val arrays = project.in(file("arrays"))
