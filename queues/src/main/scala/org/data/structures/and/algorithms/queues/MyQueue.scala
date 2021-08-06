package org.data.structures.and.algorithms.queues

import scala.annotation.tailrec
import scala.reflect.ClassTag

class MyQueue[T](maxSize: Int, implicit val ct: ClassTag[T]) {

  private val queue = Array.ofDim[T](maxSize)

  private var front = -1

  private var rear = 0

  private var queueSize = 0

  def insert(t: T): Either[Throwable, Unit] = if (isFull) Left(new Exception("Queue is full!")) else {
    queue(rear) = t
    rear = (rear + 1) % maxSize
    queueSize = queueSize + 1
    Right(())
  }

  def remove(): Either[Throwable, T] = if (isEmpty) Left(new Exception("Queue is empty!")) else {
    front = (front + 1) % maxSize
    val t = queue(front)
    queueSize = queueSize - 1
    Right(t)
  }

  def peek: Option[T] =
    if (isEmpty) None else Some(queue(front + 1))

  def iterator: Iterator[T] = {
    @tailrec
    def go(arr: Array[T], n: Int = 0): Array[T] = if (n == queueSize) arr else {
      val index = (front + 1 + n) % maxSize
      arr(n) = queue(index)
      go(arr, n + 1)
    }

    go(Array.ofDim[T](size)).iterator
  }

  def size: Int = queueSize

  def isEmpty: Boolean =
    queueSize == 0

  def isFull: Boolean =
    queueSize == maxSize

}

object MyQueue {

  def apply[T](maxSize: Int = 3)(implicit ct: ClassTag[T]): MyQueue[T] =
    new MyQueue[T](maxSize, ct)

}