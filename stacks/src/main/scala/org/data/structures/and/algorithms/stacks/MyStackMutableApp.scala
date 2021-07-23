package org.data.structures.and.algorithms.stacks

import scala.annotation.tailrec

object MyStackMutableApp extends App {

  val myStack = MyStackMutable[Int](maxSize = 7)
  println(s"Stack size: ${myStack.size}")

  println("Pushing 3 elements...")

  @tailrec
  def goPush(elements: List[Int]): Unit = elements match {
    case Nil =>
      println(s"Stack size: ${myStack.size}")
      println(s"Stack ==> ${myStack.iterator.mkString("[", ", ", "]")}")
    case x :: xs =>
      myStack push x
      println(s"Element '${myStack.peek.get}' pushed.")
      goPush(xs)
  }

  goPush(5 :: 10 :: 20 :: Nil)

  println("Popping all elements...")

  @tailrec
  def goPop(): Unit =
    if (myStack.isEmpty) {
      println(s"Stack size: ${myStack.size}")
      println(s"Stack ==> ${myStack.iterator.mkString("[", ", ", "]")}")
    } else {
      val element = myStack.pop()
      println(s"Element '${element.toOption.get}' popped.")
      goPop()
    }

  goPop()

}
