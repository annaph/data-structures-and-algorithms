package org.data.structures.and.algorithms.sorting

import scala.annotation.tailrec

object SelectionSort {

  def sort[T](arr: Array[T])(implicit or: Ordering[T]): Array[T] =
    if (arr.isEmpty || arr.length == 1) arr else sort(arr, i = 0)

  @tailrec
  private def sort[T](arr: Array[T], i: Int)(implicit or: Ordering[T]): Array[T] =
    if (i == arr.length - 1) arr else {
      val index = targetIndex(arr, i)
      sort(swap(arr, i, index), i + 1)
    }

  private def targetIndex[T](arr: Array[T], j: Int)(implicit or: Ordering[T]): Int =
    ((j + 1) until arr.length).foldLeft(j) {
      case (index, i) =>
        if (or.lt(arr(i), arr(index))) i else index
    }

  private def swap[T](arr: Array[T], i: Int, j: Int): Array[T] = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
    arr
  }

}
