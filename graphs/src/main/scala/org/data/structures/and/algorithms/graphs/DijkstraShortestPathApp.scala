package org.data.structures.and.algorithms.graphs

import org.data.structures.and.algorithms.graphs.DijkstraShortestPathFinder.ShortestPath
import org.data.structures.and.algorithms.graphs.Implicits.{BiDirectedEdgeOps, BiDirectedStringOps, stringVertexEmpty}

object DijkstraShortestPathApp extends App {

  println("================================================")
  println("============= DIJKSTRA SHORTEST PATH ===========")
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

  // Dijkstra shortest path finder
  val shortestPathFinder = DijkstraShortestPathFinder(vertex = "P", graph)

  println("Dijkstra shortest path finder state tables:")
  println(shortestPathFinder.toPrettyString)
  println("================================================")

  // Shortest distance
  val shortestPathFromPToR = shortestPathFinder shortestPath "R"

  println("Shortest path from 'P' to 'R':")
  println(shortestPathFromPToR.get)
  println("================================================")

  assert(
    shortestPathFromPToR.get == ShortestPath(
      start = "P",
      end = "R",
      distance = 10,
      edges = List(
        BiDirectedWeightedEdge(left = "P", right = "S", value = 2),
        BiDirectedWeightedEdge(left = "S", right = "T", value = 2),
        BiDirectedWeightedEdge(left = "T", right = "R", value = 6)
      )
    ),
    message = "Shortest path from 'P' to 'R' should be 'P' -> 'S' -> 'T' -> 'R' with distance 10!"
  )

}
