package org.data.structures.and.algorithms.graphs

import org.data.structures.and.algorithms.graphs.BiDirectedWeightedGraph.{Neighbour, Neighbours}
import org.data.structures.and.algorithms.graphs.DijkstraShortestPathFinder.{Edges, ShortestPath}

import scala.annotation.tailrec
import scala.collection.mutable

case class DijkstraShortestPathFinder[V, D](vertex: V, graph: BiDirectedWeightedGraph[V, D])
                                           (implicit vertexEmpty: Empty[V],
                                            vertexOrdering: Ordering[V],
                                            distanceNumeric: Numeric[D]) {
  require(
    graph.vertices contains vertex,
    message = s"Graph does NOT contain vertex '$vertex'!"
  )

  val (vertexToShortestDistance, vertexToPrevVertex) = DijkstraShortestPathFinder.calculateMatrices(vertex, graph)

  def shortestPath(other: V): Option[ShortestPath[V, D]] =
    if (other == vertex) None else Some {
      @tailrec
      def go(lastVertex: V, acc: Edges[V, D]): Edges[V, D] = if (lastVertex == vertex) acc else {
        val prevVertex = vertexToPrevVertex(lastVertex)
        val distance = distanceNumeric.minus(vertexToShortestDistance(lastVertex), vertexToShortestDistance(prevVertex))

        val edge = BiDirectedWeightedEdge(
          left = prevVertex,
          right = lastVertex,
          value = distance)

        go(prevVertex, edge +=: acc)
      }

      ShortestPath(
        start = vertex,
        end = other,
        distance = vertexToShortestDistance(other),
        edges = go(other, mutable.ListBuffer.empty).toList
      )
    }

  def toPrettyString: String = {
    def pairToString[Key, Value](pair: (Key, Value)): String = pair match {
      case (key, value) =>
        s"$key -> $value"
    }

    s"""
       |Vertex to shortest distance ==>
       |\t${vertexToShortestDistance.toSeq.sortBy(_._1).map(pairToString) mkString "\n\t"}
       |Vertex to prev vertex ==>
       |\t${vertexToPrevVertex.toSeq.sortBy(_._1).map(pairToString) mkString "\n\t"}
       |""".stripMargin
  }

}

object DijkstraShortestPathFinder {

  type Edges[V, D] = mutable.ListBuffer[BiDirectedWeightedEdge[V, D]]

  private def calculateMatrices[V, D](vertex: V,
                                      graph: BiDirectedWeightedGraph[V, D])
                                     (implicit vertexEmpty: Empty[V],
                                      distanceNumeric: Numeric[D]): (Map[V, D], Map[V, V]) = {
    val unvisitedVertices = mutable.Set
      .empty
      .addAll(graph.vertices)

    val vertexToShortestDistance = unvisitedVertices.foldLeft(mutable.Map.empty[V, D]) {
      case (map, unvisitedVertex) =>
        map.addOne(unvisitedVertex, distanceNumeric.zero)
    }

    val vertexToPrevVertex = unvisitedVertices.foldLeft(mutable.Map.empty[V, V]) {
      case (map, unvisitedVertex) =>
        map.addOne(unvisitedVertex, vertexEmpty.empty)
    }

    processGraph(unvisitedVertices, vertexToShortestDistance, vertexToPrevVertex)(graph, vertex)

    (vertexToShortestDistance.toMap, vertexToPrevVertex.toMap)
  }

  private def processGraph[V, D](unvisitedVertices: mutable.Set[V],
                                 vertexToShortestDistance: mutable.Map[V, D],
                                 vertexToPrevVertex: mutable.Map[V, V])
                                (graph: BiDirectedWeightedGraph[V, D], vertex: V)
                                (implicit vertexEmpty: Empty[V], distanceNumeric: Numeric[D]): Unit = {
    @tailrec
    def go(vertexWithShortestDistance: V, shortestDistance: D): Unit = {
      unvisitedNeighbours(graph, unvisitedVertices)(vertexWithShortestDistance)
        .foreach {
          updateMatrices(vertexWithShortestDistance, shortestDistance)(vertexToShortestDistance, vertexToPrevVertex)
        }

      unvisitedVertices remove vertexWithShortestDistance

      if (unvisitedVertices.nonEmpty) {
        val (newVertexWithShortestDistance, newShortestDistance) = unvisitedVertices
          .filter(vertex => vertexToShortestDistance(vertex) != distanceNumeric.zero)
          .foldLeft(vertexEmpty.empty -> distanceNumeric.zero)(findShortestDistance(vertexToShortestDistance))

        go(newVertexWithShortestDistance, newShortestDistance)
      }
    }

    go(vertex, distanceNumeric.zero)
  }

  private def unvisitedNeighbours[V, D](graph: BiDirectedWeightedGraph[V, D], unvisitedVertices: mutable.Set[V])
                                       (vertex: V): Neighbours[V, D] =
    graph
      .neighbours(vertex)
      .filter(neighbour => unvisitedVertices contains neighbour.vertex)
      .toSet

  private def updateMatrices[V, D](vertexWithShortestDistance: V, shortestDistance: D)
                                  (vertexToShortestDistance: mutable.Map[V, D], vertexToPrevVertex: mutable.Map[V, V])
                                  (neighbour: Neighbour[V, D])
                                  (implicit vertexEmpty: Empty[V], distanceNumeric: Numeric[D]): Unit = {
    val existingDistance = vertexToShortestDistance(neighbour.vertex)
    val newDistance = distanceNumeric.plus(shortestDistance, neighbour.weight)

    if (existingDistance == distanceNumeric.zero || distanceNumeric.lt(newDistance, existingDistance)) {
      vertexToShortestDistance.update(neighbour.vertex, newDistance)
      vertexToPrevVertex.update(neighbour.vertex, vertexWithShortestDistance)
    }
  }

  private def findShortestDistance[V, D](vertexToShortestDistance: mutable.Map[V, D])
                                        (currShortestDistance: (V, D), unvisitedVertex: V)
                                        (implicit distanceNumeric: Numeric[D]): (V, D) = {
    val (vertexWithShortestDistance, shortestDistance) = currShortestDistance
    val distance = vertexToShortestDistance(unvisitedVertex)

    if (distance == distanceNumeric.zero)
      vertexWithShortestDistance -> shortestDistance
    else if (shortestDistance == distanceNumeric.zero)
      unvisitedVertex -> distance
    else if (distanceNumeric.lt(distance, shortestDistance))
      unvisitedVertex -> distance
    else
      vertexWithShortestDistance -> shortestDistance
  }

  case class ShortestPath[V, D](start: V,
                                end: V,
                                distance: D,
                                edges: List[BiDirectedWeightedEdge[V, D]]) {

    override def toString: String =
      s"""
         |start:    '$start'
         |end:      '$end'
         |distance: '$distance'
         |edges:    '${edges mkString " || "}'
         |""".stripMargin

  }

}
