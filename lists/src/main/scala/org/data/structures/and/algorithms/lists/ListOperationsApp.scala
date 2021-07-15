package org.data.structures.and.algorithms.lists

object ListOperationsApp extends App {

  val l = List(1, 2, 3)
  println(s"List ==> ${l mkString ", "}")

  println(s"l(1): ${atIndex(1, l)}")
  println(s"avg(l): ${avg(l)}")
  println(s"reverse(l): ${reverse(l)}")
  println(s"length(l): ${length(l)}")

  def atIndex[T](i: Int, l: List[T]): T = l(i)

  def avg(l: List[Int]): Double = l.sum / l.size

  def reverse[T](l: List[T]): List[T] = l.reverse

  def lastElement[T](l: List[T]): T = l.foldLeft(Option.empty[T]) {
    case (_, x) => Some(x)
  }.get

  def length[T](l: List[T]): Int = l.foldLeft(0) {
    case (acc, _) => acc + 1
  }

}
