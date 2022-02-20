package org.data.structures.and.algorithms.graphs

sealed trait Edge[T] {

  def left: T

  def right: T

}

sealed trait Weight[T] {

  def value: T

}

case class UniDirectedEdge[T](left: T, right: T) extends Edge[T] {

  override def toString: String =
    s"$left -> $right"

}

case class BiDirectedEdge[T](left: T, right: T)(implicit ord: Ordering[T]) extends Edge[T] {

  override def equals(that: Any): Boolean =
    that match {
      case that: BiDirectedEdge[T] =>
        lazy val condition1 = this.left == that.left && this.right == that.right
        lazy val condition2 = this.left == that.right && this.right == that.left

        that.canEqual(obj = this) &&
          (condition1 || condition2)
      case _ =>
        false
    }

  override def canEqual(obj: Any): Boolean =
    obj.isInstanceOf[BiDirectedEdge[T]]

  override def hashCode(): Int = {
    val prime = 31
    var result = 1

    val first = if (ord.lt(left, right)) left else right
    val second = if (ord.gteq(left, right)) left else right

    result = prime * result + first.hashCode()
    result = prime * result + second.hashCode()

    result
  }

  override def toString: String =
    s"$left <-> $right"

}

case class BiDirectedWeightedEdge[T, W](left: T, right: T, value: W)
                                       (implicit ord: Ordering[T]) extends Edge[T] with Weight[W] {

  override def equals(that: Any): Boolean =
    that match {
      case that: BiDirectedWeightedEdge[T, W] =>
        lazy val condition1 = this.left == that.left && this.right == that.right
        lazy val condition2 = this.left == that.right && this.right == that.left

        that.canEqual(obj = this) &&
          this.value == that.value &&
          (condition1 || condition2)
      case _ =>
        false
    }

  override def canEqual(obj: Any): Boolean =
    obj.isInstanceOf[BiDirectedWeightedEdge[T, W]]

  override def hashCode(): Int = {
    val prime = 31
    var result = 1

    val first = if (ord.lt(left, right)) left else right
    val second = if (ord.gteq(left, right)) left else right

    result = prime * result + first.hashCode()
    result = prime * result + second.hashCode()
    result = prime * result + value.hashCode()

    result
  }

  override def toString: String =
    s"$left <-$value-> $right"

}
