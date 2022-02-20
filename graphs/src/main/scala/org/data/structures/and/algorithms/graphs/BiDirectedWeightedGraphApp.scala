package org.data.structures.and.algorithms.graphs

import org.data.structures.and.algorithms.graphs.BiDirectedWeightedGraph.Neighbour
import org.data.structures.and.algorithms.graphs.Implicits.{BiDirectedEdgeOps, BiDirectedStringOps}

object BiDirectedWeightedGraphApp extends App {

  println("================================================")
  println("=========== BI DIRECTED WEIGHTED GRAPH =========")
  println("================================================")

  // Create graph
  val graph = BiDirectedWeightedGraph.empty[String, Int]
    .addEdge(edge = "P" <-> "Q" | 7)
    .addEdge(edge = "P" <-> "S" | 2)
    .addEdge(edge = "Q" <-> "R" | 6)
    .addEdge(edge = "Q" <-> "S" | 3)
    .addEdge(edge = "Q" <-> "T" | 3)
    .addEdge(edge = "R" <-> "T" | 6)
    .addEdge(edge = "S" <-> "T" | 2)

  println("Graph:")
  println(graph.toPrettyString)
  println("================================================")

  // List vertices
  val vertices = graph.vertices.toList.sorted

  println(s"Vertices: ${vertices mkString ", "}")
  println("================================================")

  assert(
    vertices == List("P", "Q", "R", "S", "T"),
    message = "Graph does NOT contain correct vertices!"
  )

  // List edges
  val edges = graph.edges.toSet

  println("Edges:")
  println(edges mkString "\n")
  println("================================================")

  assert(
    edges == Set(
      BiDirectedWeightedEdge("P", "Q", 7), BiDirectedWeightedEdge("P", "S", 2), BiDirectedWeightedEdge("Q", "R", 6),
      BiDirectedWeightedEdge("Q", "S", 3), BiDirectedWeightedEdge("Q", "T", 3), BiDirectedWeightedEdge("R", "T", 6),
      BiDirectedWeightedEdge("S", "T", 2)),
    message = "Graph does NOT contain correct edges!"
  )

  // List neighbours
  val neighbours = graph.neighbours(vertex = "T").toSet

  println(s"Neighbours of vertex 'T': ${neighbours mkString ", "}")
  println("================================================")

  assert(
    neighbours == Set(Neighbour("Q", 3), Neighbour("R", 6), Neighbour("S", 2)),
    message = "Neighbours of vertex 'T' should be: 'Q' -> 3, 'R' -> 6 and 'S' -> 2!"
  )

}
