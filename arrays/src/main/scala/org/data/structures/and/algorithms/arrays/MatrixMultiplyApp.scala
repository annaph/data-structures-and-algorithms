package org.data.structures.and.algorithms.arrays

object MatrixMultiplyApp extends App {

  val a: Array[Array[Double]] = Array(
    Array(2.5, 1.5, 0.5),
    Array(1, 2, 4)
  )

  val b: Array[Array[Double]] = Array(
    Array(-1, -1.5, 1, -1),
    Array(0.5, -2, -2.5, 1),
    Array(1, 2, 1, 1)
  )

  val result = multiplyMatrices(a, b)

  for (arr <- result) println(arr.mkString("[", ", ", "]"))

  def multiplyMatrices(a: Array[Array[Double]], b: Array[Array[Double]]): Array[Array[Double]] = {
    val numOfRowsA = a.length
    val numOfColumnsA = a(0).length

    val numOfRowsB = b.length
    val numOfColumnsB = b(0).length

    if (numOfColumnsA != numOfRowsB) throw new Exception("Cannot multiply matrices") else {
      val c = Array.ofDim[Double](numOfRowsA, numOfColumnsB)

      for {
        i <- 0 until numOfRowsA
        j <- 0 until numOfColumnsB
        k <- 0 until numOfColumnsA
      } c(i)(j) = c(i)(j) + a(i)(k) * b(k)(j)

      c
    }
  }

}
