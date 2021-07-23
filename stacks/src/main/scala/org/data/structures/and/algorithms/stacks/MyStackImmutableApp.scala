package org.data.structures.and.algorithms.stacks

import scala.annotation.tailrec

object MyStackImmutableApp extends App {

  val myStack = MyStackImmutable[Int](maxSize = 7)
  println(s"Stack size: ${myStack.size}")

  println("Pushing 3 elements...")

  @tailrec
  def goPush(elements: List[Int], stack: MyStackImmutable[Int]): MyStackImmutable[Int] = elements match {
    case Nil =>
      stack
    case x :: xs =>
      val newStack = pushToStack(stack, x)
      println(s"Element '${newStack.peek.get}' pushed.")
      goPush(xs, newStack)
  }

  val myNewStack = goPush(List(5, 10, 20), myStack)

  println(s"Stack size: ${myNewStack.size}")
  println(s"Stack ==> ${myNewStack.iterator.mkString("[", ", ", "]")}")

  println("Popping all elements...")

  @tailrec
  def goPop(stack: MyStackImmutable[Int]): MyStackImmutable[Int] = if (stack.isEmpty) stack else {
    val (element, newStack) = popFromStack(stack)
    println(s"Element '$element' popped.")
    goPop(newStack)
  }

  val myNewStack2 = goPop(myNewStack)

  println(s"Stack size: ${myNewStack2.size}")
  println(s"Stack ==> ${myNewStack2.iterator.mkString("[", ", ", "]")}")

  private def pushToStack[T](stack: MyStackImmutable[T], t: T): MyStackImmutable[T] =
    stack.push(t) match {
      case Left(e) =>
        throw e
      case Right(newStack) =>
        newStack
    }

  private def popFromStack[T](stack: MyStackImmutable[T]): (T, MyStackImmutable[T]) =
    stack.pop() match {
      case Left(e) =>
        throw e
      case Right((t, newStack)) =>
        t -> newStack
    }

}
