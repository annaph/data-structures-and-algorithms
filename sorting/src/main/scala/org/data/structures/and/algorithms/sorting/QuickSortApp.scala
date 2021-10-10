package org.data.structures.and.algorithms.sorting

import org.data.structures.and.algorithms.sorting.QuickSort.sort

object QuickSortApp extends App {

  // sort numbers
  val numbers = List(5, 2, 1, 6, 7)

  val ascendingNumbers = sort(numbers)(AscendingIntOrder)
  println(s"Numbers ascending: ${ascendingNumbers mkString ", "}")

  assert(
    ascendingNumbers == List(1, 2, 5, 6, 7),
    message = "numbers ascending sort is not correct!"
  )

  val descendingNumbers = sort(numbers)(DescendingIntOrder)
  println(s"numbers descending: ${descendingNumbers mkString ", "}")

  assert(
    descendingNumbers == List(7, 6, 5, 2, 1),
    message = "numbers descending sort is not correct!"
  )

  // sort strings
  val strings = List("grape", "apple", "apricot")

  val ascendingStrings = sort(strings)(AscendingStringOrder)
  println(s"strings ascending: ${ascendingStrings mkString ", "}")

  assert(
    ascendingStrings == List("apple", "apricot", "grape"),
    message = "strings ascending sort is not correct!"
  )

  val descendingStrings = sort(strings)(DescendingStringOrder)
  println(s"strings descending: ${descendingStrings mkString ", "}")

  assert(
    descendingStrings == List("grape", "apricot", "apple"),
    message = "strings descending sort is not correct!"
  )

}
