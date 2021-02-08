package org.data.structures.and.algorithms.fundamental

object DecimalToBinaryConvApp extends App {

  val n1 = 5
  val result1 = toBin(n1)

  println(result1)

  val n2 = 8
  val result2 = toBin(n2)

  println(result2)

  def toBin(n: Int): String =
    Iterator.iterate(n)(_ / 2)
      .takeWhile(_ > 0)
      .map(_ % 2)
      .mkString.reverse

}
