package org.data.structures.and.algorithms.foundation

object StreamsApp extends App {

  val myFibonacciSeries = createFibonacciSeries(0, 1)

  myFibonacciSeries.take(7).foreach(println)

  def createInfStream(x: Int): LazyList[Int] = {
    println("Processing...")
    LazyList.cons(x, createInfStream(x + 1))
  }

  def createFibonacciSeries(a: Int, b: Int): LazyList[Int] =
    LazyList.cons(a, createFibonacciSeries(b, a + b))

}
