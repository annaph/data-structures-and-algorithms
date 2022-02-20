package org.data.structures.and.algorithms.graphs

case class BiDirectedGraph[V](protected val adjacencyMap: Map[V, Set[V]])(implicit ord: Ordering[V])
  extends Graph[V, BiDirectedEdge[V], V]
    with GraphInternalRepr[V, V]
    with GraphOps[V, BiDirectedEdge[V], V] {

  override def vertices: Iterator[V] = allVertices

  override def edges: Iterator[BiDirectedEdge[V]] =
    allEdges(BiDirectedEdge(_, _))

  override def addEdge(edge: BiDirectedEdge[V]): BiDirectedGraph[V] =
    BiDirectedGraph(addEdge(
      adjacencyMap,
      source1 = edge.left,
      source2 = edge.right,
      source1Neighbour = edge.right,
      source2Neighbour = Some(edge.left))
    )

  override def neighbours(vertex: V): Iterator[V] =
    allNeighbours(vertex)

}

object BiDirectedGraph {

  def empty[V](implicit ord: Ordering[V]): BiDirectedGraph[V] =
    BiDirectedGraph(adjacencyMap = Map.empty[V, Set[V]])

  def apply[V](edges: BiDirectedEdge[V]*)(implicit ord: Ordering[V]): BiDirectedGraph[V] =
    edges.foldLeft(BiDirectedGraph.empty[V]) {
      case (graph, edge) =>
        graph addEdge edge
    }

}
