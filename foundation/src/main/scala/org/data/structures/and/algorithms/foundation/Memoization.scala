package org.data.structures.and.algorithms.foundation

import scala.collection.mutable

object MemoizationApp extends App {
  val x = 3
  val result = Memoization factorial x
  println(s"Result: $result")

  val factorialMemoize = new FactorialMemoize

  val result2 = factorialMemoize factorial 3
  println(s"Result2: $result2")

  val result3 = factorialMemoize factorial 4
  println(s"Result3: $result3")

}

object Memoization {

  def factorial(x: Int): Int =
    LazyList.from(1).take(x).product

}

class FactorialMemoize {

  private val _cache = mutable.Map.empty[Int, Int]

  def factorial(x: Int): Int = x match {
    case 0 | 1 =>
      1
    case n if _cache contains n =>
      println("Performing lookup")
      _cache(n)
    case _ =>
      println("Performing calculation")
      val n = x * factorial(x - 1)

      _cache += x -> n
      n
  }

}
