package org.data.structures.and.algorithms.graphs

import org.data.structures.and.algorithms.graphs.BiDirectedWeightedGraph.{Neighbour, Neighbours}

case class BiDirectedWeightedGraph[V, W](protected val adjacencyMap: Map[V, Neighbours[V, W]])
                                        (implicit ord: Ordering[V])
  extends Graph[V, BiDirectedWeightedEdge[V, W], Neighbour[V, W]]
    with GraphInternalRepr[V, Neighbour[V, W]]
    with GraphOps[V, BiDirectedWeightedEdge[V, W], Neighbour[V, W]] {

  override def vertices: Iterator[V] = allVertices

  override def edges: Iterator[BiDirectedWeightedEdge[V, W]] = allEdges {
    (vertex, neighbour) =>
      BiDirectedWeightedEdge(
        left = vertex,
        right = neighbour.vertex,
        value = neighbour.weight)
  }

  override def addEdge(edge: BiDirectedWeightedEdge[V, W]): BiDirectedWeightedGraph[V, W] =
    BiDirectedWeightedGraph(addEdge(
      adjacencyMap,
      source1 = edge.left,
      source2 = edge.right,
      source1Neighbour = Neighbour(edge.right, edge.value),
      source2Neighbour = Some(Neighbour(edge.left, edge.value)))
    )

  override def neighbours(vertex: V): Iterator[Neighbour[V, W]] =
    allNeighbours(vertex)

}

object BiDirectedWeightedGraph {

  type Neighbours[V, W] = Set[Neighbour[V, W]]

  def empty[V, W](implicit ord: Ordering[V]): BiDirectedWeightedGraph[V, W] =
    BiDirectedWeightedGraph(adjacencyMap = Map.empty[V, Neighbours[V, W]])

  def apply[V, W](edges: BiDirectedWeightedEdge[V, W]*)(implicit ord: Ordering[V]): BiDirectedWeightedGraph[V, W] =
    edges.foldLeft(BiDirectedWeightedGraph.empty[V, W]) {
      case (graph, edge) =>
        graph addEdge edge
    }

  case class Neighbour[V, W](vertex: V, weight: W) {

    override def toString: String =
      s"($vertex:$weight)"

  }

}
