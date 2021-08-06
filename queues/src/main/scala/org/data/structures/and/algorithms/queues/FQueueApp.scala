package org.data.structures.and.algorithms.queues

import scala.annotation.tailrec

object FQueueApp extends App {

  // Create queue
  println("Creating queue...")
  val myQueue1 = FQueue.empty[Int]
  printQueueInfo(myQueue1)

  // Insert 3 elements
  println("Inserting 3 elements...")
  val myQueue2 = goInsert(1 :: 2 :: 3 :: Nil, myQueue1)
  printQueueInfo(myQueue2)

  // Remove 2 elements
  println(s"Removing 2 elements...")
  val myQueue3 = goRemove(2, myQueue2)
  printQueueInfo(myQueue3)

  // Insert 2 elements
  println("Inserting 2 elements...")
  val myQueue4 = goInsert(4 :: 5 :: Nil, myQueue3)
  printQueueInfo(myQueue4)

  // Remove 3 elements
  println(s"Removing 3 elements...")
  val myQueue5 = goRemove(3, myQueue4)
  printQueueInfo(myQueue5)

  assert(myQueue5.isEmpty, "Queue should be empty!")

  @tailrec
  def goInsert(elements: List[Int], queue: FQueue[Int]): FQueue[Int] = elements match {
    case Nil =>
      queue
    case x :: xs =>
      val newQueue = queue.insert(x)
      goInsert(xs, newQueue)
  }

  @tailrec
  def goRemove(n: Int, queue: FQueue[Int]): FQueue[Int] = if (n == 0) queue else {
    val (x, newQueue) = queue.remove()
    println(s"Removing from queue ===> $x")
    goRemove(n - 1, newQueue)
  }

  private def printQueueInfo[T](queue: FQueue[T]): Unit = {
    println(s"Queue size ===> ${queue.size}")
    println(s"Queue peek ==> ${queue.peek}")
    println(s"Queue second peek ==> ${queue.peekSecond}")
    println(s"Queue content ===> ${queue.iterator.mkString("[", ", ", "]")}")
    println("====================================")
  }

}
