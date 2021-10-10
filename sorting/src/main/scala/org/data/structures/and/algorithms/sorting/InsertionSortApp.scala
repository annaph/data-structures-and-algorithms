package org.data.structures.and.algorithms.sorting

import org.data.structures.and.algorithms.sorting.InsertionSort.sort

object InsertionSortApp extends App {

  // sort numbers
  val numbers = List(15, 10, 33, 11)

  val ascendingNumbers = sort(numbers)(AscendingIntOrder)
  println(s"Numbers ascending: ${ascendingNumbers mkString ", "}")

  assert(
    ascendingNumbers == List(10, 11, 15, 33),
    message = "numbers ascending sort is not correct!"
  )

  val descendingNumbers = sort(numbers)(DescendingIntOrder)
  println(s"numbers descending: ${descendingNumbers mkString ", "}")

  assert(
    descendingNumbers == List(33, 15, 11, 10),
    message = "numbers descending sort is not correct!"
  )

  // sort strings
  val strings = List("banana", "apple", "mango")

  val ascendingStrings = sort(strings)(AscendingStringOrder)
  println(s"strings ascending: ${ascendingStrings mkString ", "}")

  assert(
    ascendingStrings == List("apple", "banana", "mango"),
    message = "strings ascending sort is not correct!"
  )

  val descendingStrings = sort(strings)(DescendingStringOrder)
  println(s"strings descending: ${descendingStrings mkString ", "}")

  assert(
    descendingStrings == List("mango", "banana", "apple"),
    message = "strings descending sort is not correct!"
  )

}
