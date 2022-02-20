package org.data.structures.and.algorithms.graphs

import org.data.structures.and.algorithms.graphs.MyGraph.Edge

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.collection.mutable

class MyGraph(edges: List[Edge]) {

  def addEdge(edge: Edge): MyGraph =
    new MyGraph(edge :: edges)

  def successors(vertex: String): Set[String] =
    edges
      .filter(_._1 == vertex)
      .map(_._2)
      .toSet

  def breadthFirst(start: String): Iterator[String] = {
    val traverseInfo = buildTraverseInfo(start)
    traverse(traverseInfo, range = traverseInfo.depth to 0 by -1)
  }

  def depthFirst(start: String): Iterator[String] = {
    val traverseInfo = buildTraverseInfo(start)
    traverse(traverseInfo, range = 0L to traverseInfo.depth)
  }

  def topologicalSort(failOnCycles: Boolean = false): Iterator[String] = {
    if (failOnCycles && cycles.nonEmpty) throw new Exception("Topological sort can only be performed on DAG!")

    edges.headOption
      .map(edge => breadthFirst(edge._1))
      .getOrElse(Iterator.empty)
  }

  def cycles: Iterator[String] = {
    val (_, cycleVertices) = edges.foldLeft(mutable.Set.empty[String] -> List.empty[String]) {
      case ((visited, cycles), (left, right)) =>
        val cyclesWithLeft = if (visited contains left) cycles else {
          visited add left
          if (isCycleVertex(left)) left :: cycles else cycles
        }

        val cyclesWithLeftAndRight = if (visited contains right) cyclesWithLeft else {
          visited add right
          if (isCycleVertex(right)) right :: cyclesWithLeft else cyclesWithLeft
        }

        visited -> cyclesWithLeftAndRight
    }

    cycleVertices.iterator
  }

  private def traverse(traverseInfo: TraverseInfo, range: NumericRange[Long]): Iterator[String] = {
    val result = range.foldLeft(List.empty[String]) {
      case (acc, i) =>
        traverseInfo.nodes(i).foldLeft(acc) {
          case (result, node) =>
            node :: result
        }
    }

    result.iterator
  }

  private def buildTraverseInfo(start: String): TraverseInfo = {
    @tailrec
    def go(deepestNodes: mutable.Set[String], visited: mutable.Set[String], acc: TraverseInfo): TraverseInfo =
      if (deepestNodes.isEmpty) acc else {
        val depth = acc.depth
        val nodesToRemove = mutable.Set.empty[String]

        val firstNextNodes = mutable.Set.empty[String]
        val secondNextNodes = mutable.Set.empty[String]

        for {
          node <- deepestNodes
          successor <- successors(node)
          if !visited.contains(successor)
        } {
          if (deepestNodes contains successor) {
            firstNextNodes add successor
            nodesToRemove add successor
          } else secondNextNodes add successor

          visited add successor
        }

        nodesToRemove.foreach(node => acc.nodes(depth).remove(node))

        acc.nodes.put(depth + 1, firstNextNodes)
        acc.nodes.put(depth + 2, secondNextNodes)

        go(secondNextNodes, visited, TraverseInfo(depth + 2, acc.nodes))
      }

    val initNodes = mutable.Set(start)
    go(
      initNodes,
      mutable.Set.empty,
      TraverseInfo(depth = 0, nodes = mutable.Map(0L -> initNodes))
    )
  }

  private def isCycleVertex(vertex: String): Boolean = {
    case class Result(currSuccessors: Set[String], visited: mutable.Set[String] = mutable.Set.empty)

    @tailrec
    def go(result: Result): Boolean = {
      val currSuccessors = result.currSuccessors
      val visited = result.visited

      if (visited contains vertex) true
      else if (currSuccessors.isEmpty) false else {
        val newCurrSuccessors = currSuccessors.flatMap(successors).diff(visited)
        val newVisited = visited addAll newCurrSuccessors
        go(Result(newCurrSuccessors, newVisited))
      }
    }

    val initResult = Result(Set(vertex))
    go(initResult)
  }

  private case class TraverseInfo(depth: Long,
                                  nodes: mutable.Map[Long, mutable.Set[String]])

}

object MyGraph {

  type Edge = (String, String)

  def apply(edges: Edge*): MyGraph =
    new MyGraph(edges.toList)

}
