package org.data.structures.and.algorithms.searching

import org.data.structures.and.algorithms.searching.NaiveSubstringSearch.{findAll, findFirst}

object NaiveSubstringSearchApp extends App {

  val text1 = "This is a functional implementation."
  val pattern1 = "functional"
  val result1 = findFirst(pattern1, text1)
  println(s"Result 1: $result1")

  assert(
    result1.isDefined && result1.get == 10,
    message = s"'$pattern1' should be found in the text!"
  )

  val text2 = "This is a imperative implementation."
  val pattern2 = "functional"
  val result2 = findFirst(pattern2, text2)
  println(s"Result 2: $result2")

  assert(
    result2.isEmpty,
    message = s"'$pattern2' should not be found in the text!"
  )

  val text3 = "Hi Anna! How are you, Anna? You look great Anna :)"
  val pattern3 = "Anna"
  val result3 = findAll(pattern3, text3)
  println(s"Result 3: ${result3 mkString ", "}")

  assert(
    result3 == Seq(3, 22, 43),
    message = s"'$pattern3' should be found in the text on multiply places!"
  )

}
