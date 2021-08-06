package org.data.structures.and.algorithms.queues

import scala.annotation.tailrec

object MyQueueApp extends App {

  // Create queue
  println("Creating queue...")
  val myQueue = MyQueue[Int](maxSize = 3)
  printQueueInfo()

  // Insert 3 elements
  println("Inserting 3 elements...")
  goInsert(1 :: 2 :: 3 :: Nil)
  assert(myQueue.isFull, "Queue should be full!")

  // Remove 2 elements
  println(s"Removing 2 elements...")
  goRemove(2)

  // Insert 2 elements
  println("Inserting 2 elements...")
  goInsert(4 :: 5 :: Nil)

  // Remove 3 elements
  println(s"Removing 3 elements...")
  goRemove(3)
  assert(myQueue.isEmpty, "Queue should be empty!")

  @tailrec
  def goInsert(elements: List[Int]): Unit = elements match {
    case Nil =>
      printQueueInfo()
    case x :: xs => myQueue.insert(x) match {
      case Left(e) => throw e
      case Right(_) => goInsert(xs)
    }
  }

  @tailrec
  def goRemove(n: Int): Unit = if (n == 0) printQueueInfo() else {
    myQueue.remove() match {
      case Left(e) => throw e
      case Right(x) =>
        println(s"Removing from queue ===> $x")
        goRemove(n - 1)
    }
  }

  private def printQueueInfo(): Unit = {
    println(s"Queue size ===> ${myQueue.size}")
    println(s"Queue peek ==> ${myQueue.peek}")
    println(s"Queue content ===> ${myQueue.iterator.mkString("[", ", ", "]")}")
    println("====================================")
  }

}
