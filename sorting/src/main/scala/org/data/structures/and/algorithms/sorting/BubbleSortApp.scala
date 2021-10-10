package org.data.structures.and.algorithms.sorting

import org.data.structures.and.algorithms.sorting.BubbleSort.sort

object BubbleSortApp extends App {

  val list = List(3, 1, 6, 8, 2)

  val ascending = sort(list)(AscendingIntOrder)
  println(s"Ascending: ${ascending mkString ", "}")
  assert(ascending == List(1, 2, 3, 6, 8), message = "ascending sort is not correct!")

  val descending = sort(list)(DescendingIntOrder)
  println(s"Descending: ${descending mkString ", "}")
  assert(descending == List(8, 6, 3, 2, 1), message = "descending sort is not correct!")

}
