package org.data.structures.and.algorithms.graphs

import scala.collection.mutable

trait Graph[V, E <: Edge[V], N] {

  def vertices: Iterator[V]

  def edges: Iterator[E]

  def addEdge(edge: E): Graph[V, E, N]

  def neighbours(vertex: V): Iterator[N]

}

trait GraphInternalRepr[V, N] {

  protected def adjacencyMap: Map[V, Set[N]]

}

trait GraphOps[V, E, N] {
  this: GraphInternalRepr[V, N] =>

  def allVertices: Iterator[V] =
    adjacencyMap.keys.iterator

  def allEdges(f: (V, N) => E): Iterator[E] = {
    val result = adjacencyMap.foldLeft(mutable.Set.empty[E]) {
      case (acc, (vertex, neighbours)) =>
        val newEdges = neighbours.map(f(vertex, _))
        acc addAll newEdges
    }

    result.iterator
  }

  def allNeighbours(vertex: V): Iterator[N] =
    adjacencyMap
      .getOrElse(vertex, Set.empty[N])
      .iterator

  def addEdge(adjacencyMap: Map[V, Set[N]],
              source1: V,
              source2: V,
              source1Neighbour: N,
              source2Neighbour: Option[N]): Map[V, Set[N]] = {
    val result1 = adjacencyMap.get(source1) match {
      case Some(source1Neighbours) =>
        val adjacency = source1 -> (source1Neighbours + source1Neighbour)
        adjacencyMap + adjacency
      case None =>
        val adjacency = source1 -> Set(source1Neighbour)
        adjacencyMap + adjacency
    }

    result1.get(source2) match {
      case Some(source2Neighbours) =>
        val newSource2Neighbours = source2Neighbour.map(source2Neighbours + _) getOrElse source2Neighbours
        val adjacency = source2 -> newSource2Neighbours
        result1 + adjacency
      case None =>
        val newSource2Neighbours = source2Neighbour.map(Set(_)) getOrElse Set.empty
        val adjacency = source2 -> newSource2Neighbours
        result1 + adjacency
    }
  }

  def toPrettyString(implicit ordering: Ordering[V]): String = {
    val formattedAdjacencyMap = adjacencyMap
      .toSeq
      .sortBy(_._1)
      .map {
        case (key, value) =>
          s"$key -> ${value mkString ","}"
      }

    formattedAdjacencyMap mkString "\n"
  }

}
