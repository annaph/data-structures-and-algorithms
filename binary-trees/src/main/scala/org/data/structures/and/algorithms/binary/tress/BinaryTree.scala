package org.data.structures.and.algorithms.binary.tress

import scala.annotation.tailrec
import scala.math.max
import scala.reflect.ClassTag

sealed trait BinaryTree[+T] extends Traversal[T] {

  def size: Int

  def depth: Int

  def toList: List[T]

  def equal[S >: T](other: BinaryTree[S]): Boolean = (this, other) match {
    case (Node(value1, left1, right1), Node(value2, left2, right2)) =>
      (value1 == value2) && (left1 equal left2) && (right1 equal right2)
    case (Leaf(value1), Leaf(value2)) =>
      value1 == value2
    case (Empty, Empty) =>
      true
    case _ =>
      false
  }

  def flip: BinaryTree[T] with Traversal[T] = this match {
    case Node(_, Empty, _) =>
      throw new Exception("Cannot flip node!")

    case Node(value, Leaf(leafValue), right) =>
      Node(leafValue, right, Leaf(value))

    case Node(value, left, right) =>
      (left.flip: @unchecked) match {
        case Node(flippedParentValue, flippedLeft, Leaf(flippedRightValue)) =>
          Node(flippedParentValue, flippedLeft, Node(flippedRightValue, right, Leaf(value)))
      }

    case Empty =>
      Empty

    case x: Leaf[T] =>
      x
  }

  def isFlipped[S >: T](other: BinaryTree[S]): Boolean =
    other.flip equal this

}

case class Node[T](value: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T] {

  override def size: Int =
    1 + left.size + right.size

  override def depth: Int =
    1 + max(left.depth, right.depth)

  override def toList: List[T] =
    value :: left.toList ::: right.toList

  override def preorder: Iterator[T] =
    this.toList.iterator

  override def inorder: Iterator[T] =
    (left.inorder.toList ::: List(value) ::: right.inorder.toList).iterator

  override def postorder: Iterator[T] =
    (left.postorder.toList ::: right.postorder.toList ::: List(value)).iterator

}

case class Leaf[+T](value: T) extends BinaryTree[T] {

  override val size: Int = 1

  override val depth: Int = 0

  override def toList: List[T] =
    value :: Nil

  override def preorder: Iterator[T] = oneValueIterator

  override def inorder: Iterator[T] = oneValueIterator

  override def postorder: Iterator[T] = oneValueIterator

  private def oneValueIterator =
    Iterator(value)

}

case object Empty extends BinaryTree[Nothing] {

  override val size: Int = 0

  override val depth: Int = 0

  override def toList: List[Nothing] =
    List.empty[Nothing]

  override def preorder: Iterator[Nothing] = emptyIterator

  override def inorder: Iterator[Nothing] = emptyIterator

  override def postorder: Iterator[Nothing] = emptyIterator

  private def emptyIterator: Iterator[Nothing] =
    Iterator.empty

}

object BinaryTree {

  type Dictionary[T] = BinaryTree[(String, T)]

  def apply[T](values: T*): BinaryTree[T] with Traversal[T] = values.toList match {
    case Nil =>
      Empty
    case x :: Nil =>
      Leaf(x)
    case x :: xs =>
      val halfLength = (xs.length + 1) / 2
      Node(
        value = x,
        left = BinaryTree(xs take halfLength: _*),
        right = BinaryTree(xs drop halfLength: _*)
      )
  }

  def complete[T](values: T*)(implicit ct: ClassTag[T]): BinaryTree[T] with Traversal[T] = {
    val arr = values.toArray.map(new MutableBinaryTree[T](_))

    arr.length match {
      case 0 => Empty
      case 1 => Leaf(arr.head.value)
      case _ =>
        @tailrec
        def go(i: Int = 0, j: Int = 1): Unit = if (i < (arr.length / 2) + 1) {
          val curr = arr(i)
          curr.left = if (j < arr.length) Some(arr(j)) else None
          curr.right = if (j + 1 < arr.length) Some(arr(j + 1)) else None
          go(i + 1, j + 2)
        }

        go()
        arr.head.toBinaryTree
    }
  }

  def dictionary[T](words: (String, T)*): Dictionary[T] =
    words.foldLeft[Dictionary[T]](Empty) {
      case (dict, (word, value)) =>
        dict.insert(word, value) match {
          case Left(e) =>
            throw e
          case Right(newDict) =>
            newDict
        }
    }

  implicit class DictionaryOps[T](dictionary: Dictionary[T]) {

    def get(key: String): Option[T] = dictionary match {
      case Empty =>
        None
      case Leaf((k, v)) if k == key =>
        Some(v)
      case Leaf(_) =>
        None
      case Node((k, v), _, _) if k == key =>
        Some(v)
      case Node((k, _), left, _) if key < k =>
        left get key
      case Node(_, _, right) =>
        right get key
    }

    def insert(key: String, value: T): Either[Throwable, Dictionary[T]] = dictionary match {
      case Empty =>
        Right(Leaf(key -> value))
      case Leaf((k, v)) if key < k =>
        Right(Node(k -> v, Leaf(key -> value), Empty))
      case Leaf((k, v)) =>
        Right(Node(k -> v, Empty, Leaf(key -> value)))
      case Node((k, _), _, _) if key == k =>
        Left(new Exception(s"key '$key' already present!"))
      case Node((k, v), left, right) if key < k =>
        left.insert(key, value).map(Node(k -> v, _, right))
      case Node((k, v), left, right) =>
        right.insert(key, value).map(Node(k -> v, left, _))
    }

    def update(key: String, value: T): Either[Throwable, Dictionary[T]] = dictionary match {
      case Empty =>
        Left(new Exception(s"key '$key' cannot be updated!"))
      case Leaf((k, _)) if key == k =>
        Right(Leaf(k, value))
      case Leaf(_) =>
        Left(new Exception(s"key '$key' cannot be updated!"))
      case Node((k, _), left, right) if key == k =>
        Right(Node(k -> value, left, right))
      case Node((k, v), left, right) if key < k =>
        left.update(key, value).map(Node(k -> v, _, right))
      case Node((k, v), left, right) =>
        right.update(key, value).map(Node(k -> v, left, _))
    }

  }

  def printInfo[T](traversableBinaryTree: BinaryTree[T], message: String = ""): Unit = {
    println("====================================")
    if (message.nonEmpty) println(s"=== $message ===")
    println(s"Content: $traversableBinaryTree")
    println(s"Preorder: ${traversableBinaryTree.preorder mkString ", "}")
    println(s"Inorder: ${traversableBinaryTree.inorder mkString ", "}")
    println(s"Postorder: ${traversableBinaryTree.postorder mkString ", "}")
    println(s"Size: ${traversableBinaryTree.size}")
    println(s"Depth: ${traversableBinaryTree.depth}")
    println("====================================")
  }

}
