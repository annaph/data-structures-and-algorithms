package org.data.structures.and.algorithms

package object graphs {

  trait Empty[V] {
    def empty: V
  }

  object Implicits {

    implicit class BiDirectedStringOps(str: String) {

      def ->(other: String): UniDirectedEdge[String] =
        UniDirectedEdge(left = str, right = other)

      def <->(other: String): BiDirectedEdge[String] =
        BiDirectedEdge(left = str, right = other)

    }

    implicit class BiDirectedEdgeOps(edge: BiDirectedEdge[String]) {

      def |(weight: Int): BiDirectedWeightedEdge[String, Int] =
        BiDirectedWeightedEdge(left = edge.left, right = edge.right, value = weight)

    }

    implicit val stringVertexEmpty: Empty[String] = new Empty[String] {

      override def empty: String = ""

    }

  }

}
