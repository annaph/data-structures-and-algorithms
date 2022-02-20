package org.data.structures.and.algorithms.graphs

import scala.util.{Failure, Success, Try}

object MyGraphApp extends App {

  // Create graph
  val graph = MyGraph(
    "a" -> "b",
    "a" -> "c",
    "b" -> "c",
    "b" -> "e",
    "b" -> "d",
    "b" -> "f",
    "c" -> "e",
    "d" -> "e",
    "d" -> "f",
    "e" -> "g",
    "f" -> "h"
  )

  // Get successors
  val successorsOfA = graph successors "a"
  println(s"Successors of a: ${successorsOfA mkString ", "}")

  assert(
    successorsOfA == Set("b", "c"),
    message = s"'$successorsOfA' should be 'b' and 'c'!"
  )

  assert(
    graph.successors(vertex = "x").isEmpty,
    message = "There should not be any successors!"
  )

  // Breadth traverse
  val breadthFirst = graph breadthFirst "a"
  println(s"Breadth first traversal: ${breadthFirst mkString ", "}")

  // Depth traverse
  val depthFirst = graph depthFirst "a"
  println(s"Depth first traversal: ${depthFirst mkString ", "}")

  // Create educational path graph
  val educationalPathGraph = MyGraph(
    "prepare_test" -> "take_test",
    "take_test" -> "apply",
    "apply" -> "receive_offer",
    "receive_offer" -> "join_CMU",
    "join_CMU" -> "choose_major",
    "choose_major" -> "graduate",
    "receive_offer" -> "join_MIT",
    "join_MIT" -> "choose_major",
    "choose_major" -> "graduate"
  )

  // Topological sort
  val topologicalSort = educationalPathGraph.topologicalSort()
  println(s"Topological sort: ${topologicalSort mkString ","}")

  // Cycle detection
  val cycleGraph = educationalPathGraph
    .addEdge(edge = "choose_major" -> "join_MIT")

  val cycleVertices = cycleGraph.cycles.toSet
  println(s"Cycle vertices: ${cycleVertices mkString ","}")

  assert(
    cycleVertices == Set("join_MIT", "choose_major"),
    message = s"Cycle vertices 'join_MIT' and 'choose_major' should be detected!"
  )

  Try(cycleGraph.topologicalSort(failOnCycles = true)) match {
    case Success(_) =>
      assert(assertion = false, message = "Cycle graph should fail on topological sort!")
    case Failure(_) =>
      ()
  }

}
