package org.data.structures.and.algorithms.graphs

case class UniDirectedGraph[V](protected val adjacencyMap: Map[V, Set[V]])
  extends Graph[V, UniDirectedEdge[V], V]
    with GraphInternalRepr[V, V]
    with GraphOps[V, UniDirectedEdge[V], V] {

  override def vertices: Iterator[V] = allVertices

  override def edges: Iterator[UniDirectedEdge[V]] =
    allEdges(UniDirectedEdge(_, _))

  override def addEdge(edge: UniDirectedEdge[V]): UniDirectedGraph[V] =
    UniDirectedGraph(addEdge(
      adjacencyMap,
      source1 = edge.left,
      source2 = edge.right,
      source1Neighbour = edge.right,
      source2Neighbour = None
    ))

  override def neighbours(vertex: V): Iterator[V] =
    allNeighbours(vertex)

}

object UniDirectedGraph {

  def empty[V]: UniDirectedGraph[V] =
    UniDirectedGraph[V](adjacencyMap = Map.empty[V, Set[V]])

  def apply[V](edges: UniDirectedEdge[V]*): UniDirectedGraph[V] =
    edges.foldLeft(UniDirectedGraph.empty[V]) {
      case (graph, edge) =>
        graph addEdge edge
    }

}
