package org.data.structures.and.algorithms.sorting

object InsertionSort {

  def sort[T](list: List[T])(implicit or: Ordering[T]): List[T] = list match {
    case Nil =>
      Nil
    case _ =>
      sort(list, List.empty[T])
  }

  private def sort[T](list: List[T], sublist: List[T])(implicit or: Ordering[T]): List[T] =
    list.foldLeft(sublist) {
      case (acc, element) =>
        insert(element, acc)
    }

  private def insert[T](element: T, list: List[T])(implicit or: Ordering[T]): List[T] = list match {
    case Nil =>
      List(element)
    case head :: _ if or.lt(element, head) =>
      element :: list
    case head :: tail =>
      head :: insert(element, tail)
  }

}
