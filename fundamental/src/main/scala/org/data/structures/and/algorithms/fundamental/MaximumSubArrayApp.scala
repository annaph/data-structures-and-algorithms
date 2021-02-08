package org.data.structures.and.algorithms.fundamental

object MaximumSubArrayApp extends App {

  val data1 = Vector(-3, 1, -8, 4, -1, 2, 1, -5, 5)
  val result1 = contSubArrayMax(data1)
  println(result1)

  val data2 = Vector(1, -2, 5, 6, -1, 4, 9, -3, 2, 5)
  val result2 = contSubArrayMax(data2)
  println(result2)

  def contSubArrayMax(data: Vector[Int]): Int = data match {
    case Vector() =>
      0
    case Vector(head) =>
      head
    case _ =>
      val (left, right) = data.splitAt(data.length / 2)

      val leftMax = contSubArrayMax(left)
      val rightMax = contSubArrayMax(right)
      val middleMax = middleSubArrayMax(left, right)

      List(leftMax, middleMax, rightMax).max
  }

  private def middleSubArrayMax(left: Vector[Int], right: Vector[Int]): Int = {
    val leftSums = for (i <- 1 to left.length) yield left.takeRight(i).sum
    val rightSums = for (i <- 1 to right.length) yield right.take(i).sum

    leftSums.max + rightSums.max
  }

}
