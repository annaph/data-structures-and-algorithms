package org.data.structures.and.algorithms.sorting

import org.data.structures.and.algorithms.sorting.MergeSort.sort

object MergeSortApp extends App {

  // sort numbers
  val numbers = List(5, 6, 2, 3, 1)

  val ascendingNumbers = sort(numbers)(AscendingIntOrder)
  println(s"Numbers ascending: ${ascendingNumbers mkString ", "}")

  assert(
    ascendingNumbers == List(1, 2, 3, 5, 6),
    message = "numbers ascending sort is not correct!"
  )

  val descendingNumbers = sort(numbers)(DescendingIntOrder)
  println(s"numbers descending: ${descendingNumbers mkString ", "}")

  assert(
    descendingNumbers == List(6, 5, 3, 2, 1),
    message = "numbers descending sort is not correct!"
  )

  // sort strings
  val strings = List("cat", "put", "bag")

  val ascendingStrings = sort(strings)(AscendingStringOrder)
  println(s"strings ascending: ${ascendingStrings mkString ", "}")

  assert(
    ascendingStrings == List("bag", "cat", "put"),
    message = "strings ascending sort is not correct!"
  )

  val descendingStrings = sort(strings)(DescendingStringOrder)
  println(s"strings descending: ${descendingStrings mkString ", "}")

  assert(
    descendingStrings == List("put", "cat", "bag"),
    message = "strings descending sort is not correct!"
  )

}
