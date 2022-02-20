package org.data.structures.and.algorithms.graphs

import org.data.structures.and.algorithms.graphs.Implicits.BiDirectedStringOps

object BiDirectedGraphApp extends App {

  println("================================================")
  println("============== BI DIRECTED GRAPH ===============")
  println("================================================")

  // Create graph
  val graph = BiDirectedGraph.empty[String]
    .addEdge(edge = "P" <-> "Q")
    .addEdge(edge = "P" <-> "S")
    .addEdge(edge = "Q" <-> "R")
    .addEdge(edge = "Q" <-> "S")
    .addEdge(edge = "Q" <-> "T")
    .addEdge(edge = "R" <-> "T")
    .addEdge(edge = "S" <-> "T")

  println("Graph:")
  println(graph)
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
      BiDirectedEdge("P", "Q"), BiDirectedEdge("P", "S"), BiDirectedEdge("Q", "R"), BiDirectedEdge("Q", "S"),
      BiDirectedEdge("Q", "T"), BiDirectedEdge("R", "T"), BiDirectedEdge("S", "T")),
    message = "Graph does NOT contain correct edges!"
  )

  // List neighbours
  val neighbours = graph.neighbours(vertex = "T").toList.sorted

  println(s"Neighbours of vertex 'T': ${neighbours mkString ", "}")
  println("================================================")

  assert(
    neighbours == List("Q", "R", "S"),
    message = "Neighbours of vertex 'T' should be: 'Q', 'R' and 'S'!"
  )

}
