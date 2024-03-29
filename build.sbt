ThisBuild / name := "data-structures-and-algorithms"

ThisBuild / organization := "org.data.structures.and.algorithms"

ThisBuild / description := "Data Structures and Algorithms"

ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "2.13.4"

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:implicitConversions"
)

ThisBuild / libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter-core" % "0.21"
)

ThisBuild / fork := true
ThisBuild / connectInput in run := true

lazy val root = project.in(file(".")).aggregate(
  foundation,
  fundamental,
  arrays,
  lists,
  stacks,
  queues,
  hashTables,
  binaryTrees,
  sorting,
  searching
)

lazy val foundation = project.in(file("foundation"))

lazy val fundamental = project.in(file("fundamental"))

lazy val arrays = project.in(file("arrays"))

lazy val lists = project.in(file("lists"))

lazy val stacks = project.in(file("stacks"))

lazy val queues = project.in(file("queues"))

lazy val hashTables = project.in(file("hash-tables"))

lazy val binaryTrees = project.in(file("binary-trees"))

lazy val sorting = project.in(file("sorting"))

lazy val searching = project.in(file("searching"))

lazy val graphs = project.in(file("graphs"))
