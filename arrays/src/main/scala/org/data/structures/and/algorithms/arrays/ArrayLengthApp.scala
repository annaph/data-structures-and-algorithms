package org.data.structures.and.algorithms.arrays

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object ArrayLengthApp extends App {

  val arr1 = Array(1, 2, 3)
  val result1 = calcLength(arr1)
  println(result1)

  val arr2 = Array(1)
  val result2 = calcLength(arr2)
  println(result2)

  val arr3 = Array()
  val result3 = calcLength(arr3)
  println(result3)

  def calcLength[T](arr: Array[T]): Int = {
    @tailrec
    def go(i: Int): Int = {
      Try(arr(i)) match {
        case Success(_) =>
          go(i + 1)
        case Failure(_: ArrayIndexOutOfBoundsException) =>
          i
        case _ =>
          throw new UnsupportedOperationException
      }
    }

    go(0)
  }

}
