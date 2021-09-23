package org.data.structures.and.algorithms.binary.tress

trait Traversal[+T] {

  def preorder: Iterator[T]

  def inorder: Iterator[T]

  def postorder: Iterator[T]

}
