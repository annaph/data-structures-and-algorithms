package org.data.structures.and.algorithms.sorting

object QuickSort {

  def sort[T](list: List[T])(implicit or: Ordering[T]): List[T] = list match {
    case Nil =>
      Nil
    case head :: Nil =>
      List(head)
    case pivot :: tail =>
      val (left, right) = partition(pivot, tail)
      val leftSorted = sort(left)
      val rightSorted = sort(right)
      leftSorted ::: (pivot :: rightSorted)
  }

  private def partition[T](pivot: T,
                           list: List[T],
                           acc: (List[T], List[T]) = List.empty[T] -> List.empty[T])
                          (implicit or: Ordering[T]): (List[T], List[T]) =
    list.foldLeft(acc) {
      case ((left, right), element) if or.lt(element, pivot) =>
        (element :: left) -> right
      case ((left, right), element) =>
        left -> (element :: right)
    }

}
