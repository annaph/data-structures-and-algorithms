package org.data.structures.and.algorithms.sorting

import scala.annotation.tailrec

object MergeSort {

  def sort[T](list: List[T])(implicit or: Ordering[T]): List[T] = list match {
    case Nil =>
      Nil
    case head :: Nil =>
      List(head)
    case _ =>
      val (left, right) = split(list)
      val leftSorted = sort(left)
      val rightSorted = sort(right)
      merge(leftSorted, rightSorted)
  }

  @tailrec
  private def split[T](list: List[T],
                       placeLeft: Boolean = true,
                       acc: (List[T], List[T]) = List.empty[T] -> List.empty[T]): (List[T], List[T]) = list match {
    case Nil =>
      acc
    case head :: tail if placeLeft =>
      val (left, right) = acc
      split(tail, placeLeft = false, (head :: left) -> right)
    case head :: tail =>
      val (left, right) = acc
      split(tail, placeLeft = true, left -> (head :: right))
  }

  private def merge[T](l1: List[T], l2: List[T])(implicit or: Ordering[T]): List[T] = (l1, l2) match {
    case (Nil, _) =>
      l2
    case (_, Nil) =>
      l1
    case (head1 :: tail1, head2 :: _) if or.lteq(head1, head2) =>
      head1 :: merge(tail1, l2)
    case (_, head2 :: tail2) =>
      head2 :: merge(l1, tail2)
  }

}
