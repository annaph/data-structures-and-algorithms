package org.data.structures.and.algorithms.sorting

import org.data.structures.and.algorithms.sorting.SelectionSort.sort

object SelectionSortApp extends App {

  // Sort numbers
  val numbers = Array(5, 3, 2, 8, 7)

  val ascendingNumbers = sort(numbers)(AscendingIntOrder)
  println(s"Numbers ascending: ${ascendingNumbers mkString ", "}")

  assert(
    ascendingNumbers sameElements Array(2, 3, 5, 7, 8),
    message = "numbers ascending sort is not correct!"
  )

  val descendingNumbers = sort(numbers)(DescendingIntOrder)
  println(s"Numbers descending: ${descendingNumbers mkString ", "}")

  assert(
    descendingNumbers sameElements Array(8, 7, 5, 3, 2),
    message = "numbers descending sort is not correct!"
  )

  // Sort characters
  val characters = Array('k', 'd', 'c', 'a', 'q')

  val ascendingCharacters = sort(characters)(AscendingCharOrder)
  println(s"Characters ascending: ${ascendingCharacters mkString ", "}")

  assert(
    ascendingCharacters sameElements Array('a', 'c', 'd', 'k', 'q'),
    message = "characters ascending sort is not correct!"
  )

  val descendingCharacters = sort(characters)(DescendingCharOrder)
  println(s"Characters descending: ${descendingCharacters mkString ", "}")

  assert(
    descendingCharacters sameElements Array('q', 'k', 'd', 'c', 'a'),
    message = "characters descending sort is not correct!"
  )

}
