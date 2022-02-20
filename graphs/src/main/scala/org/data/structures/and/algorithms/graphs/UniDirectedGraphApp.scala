package org.data.structures.and.algorithms.graphs

import org.data.structures.and.algorithms.graphs.Implicits.BiDirectedStringOps

object UniDirectedGraphApp extends App {

  println("================================================")
  println("============== UNI DIRECTED GRAPH ==============")
  println("================================================")

  // Create graph
  val graph = UniDirectedGraph.empty[String]
    .addEdge(edge = "a" -> "b")
    .addEdge(edge = "a" -> "c")
    .addEdge(edge = "b" -> "c")
    .addEdge(edge = "b" -> "d")
    .addEdge(edge = "b" -> "e")
    .addEdge(edge = "b" -> "f")
    .addEdge(edge = "c" -> "e")
    .addEdge(edge = "d" -> "e")
    .addEdge(edge = "d" -> "f")
    .addEdge(edge = "e" -> "g")
    .addEdge(edge = "f" -> "h")

  println("Graph:")
  println(graph)
  println("================================================")

  // List vertices
  val vertices = graph.vertices.toList.sorted

  println(s"Vertices: ${vertices mkString ", "}")
  println("================================================")

  assert(
    vertices == List("a", "b", "c", "d", "e", "f", "g", "h"),
    message = "Graph does NOT contain correct vertices!"
  )

  // List edges
  val edges = graph.edges.toSet

  println("Edges:")
  println(edges mkString "\n")
  println("================================================")

  assert(
    edges == Set(
      UniDirectedEdge("a", "b"), UniDirectedEdge("a", "c"), UniDirectedEdge("b", "c"), UniDirectedEdge("b", "d"),
      UniDirectedEdge("b", "e"), UniDirectedEdge("b", "f"), UniDirectedEdge("c", "e"), UniDirectedEdge("d", "f"),
      UniDirectedEdge("d", "e"), UniDirectedEdge("e", "g"), UniDirectedEdge("f", "h")
    ),
    message = "Graph does NOT contain correct edges!"
  )

  // List neighbours
  val neighbours = graph.neighbours(vertex = "b").toList.sorted

  println(s"Neighbours of vertex 'b': ${neighbours mkString ", "}")
  println("================================================")

  assert(
    neighbours == List("c", "d", "e", "f"),
    message = "Neighbours of vertex 'b' should be: 'c', 'd', 'e' and 'f'!"
  )

}
