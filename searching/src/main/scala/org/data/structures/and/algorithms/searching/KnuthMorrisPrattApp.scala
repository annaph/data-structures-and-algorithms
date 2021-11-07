package org.data.structures.and.algorithms.searching

import org.data.structures.and.algorithms.searching.KnuthMorrisPratt.{findAll, findFirst}

object KnuthMorrisPrattApp extends App {

  val text1 = "yxzyxyxyxyxzxxy"
  val pattern1 = "xyxyxzx"
  val result1 = findFirst(pattern1, text1)
  println(s"Result 1: $result1")

  assert(
    result1.isDefined && result1.get == 6,
    message = s"'$pattern1' should be found in the text!"
  )

  val text2 = "This is a functional implementation."
  val pattern2 = "functional"
  val result2 = findFirst(pattern2, text2)
  println(s"Result 2: $result2")

  assert(
    result2.isDefined && result2.get == 10,
    message = s"'$pattern2' should be found in the text!"
  )

  val text3 = "This is a imperative implementation."
  val pattern3 = "functional"
  val result3 = findFirst(pattern3, text3)
  println(s"Result 3: $result3")

  assert(
    result3.isEmpty,
    message = s"'$pattern3' should not be found in the text!"
  )

  val text4 = "Hi Anna! How are you, Anna? You look great Anna :)"
  val pattern4 = "Anna"
  val result4 = findAll(pattern4, text4)
  println(s"Result 4: ${result4 mkString ", "}")

  assert(
    result4 == Seq(3, 22, 43),
    message = s"'$pattern4' should be found in the text on multiply places!"
  )

}
