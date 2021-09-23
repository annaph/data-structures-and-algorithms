package org.data.structures.and.algorithms.binary.tress

private class MutableBinaryTree[T](val value: T,
                                   var left: Option[MutableBinaryTree[T]] = None,
                                   var right: Option[MutableBinaryTree[T]] = None) {

  def toBinaryTree: BinaryTree[T] with Traversal[T] = (left, right) match {
    case (Some(l), Some(r)) =>
      Node(value, l.toBinaryTree, r.toBinaryTree)
    case (Some(l), None) =>
      Node(value, l.toBinaryTree, Empty)
    case (None, Some(r)) =>
      Node(value, Empty, r.toBinaryTree)
    case (None, None) =>
      Leaf(value)
  }

}
