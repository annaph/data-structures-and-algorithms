package org.data.structures.and.algorithms.queues

import scala.reflect.ClassTag

class FQueue[T] private(in: List[T], out: List[T], queueSize: Int, implicit val ct: ClassTag[T]) {

  def insert(t: T): FQueue[T] =
    if (isEmpty) new FQueue[T](List.empty[T], t :: Nil, queueSize + 1, ct)
    else new FQueue[T](t :: in, out, queueSize + 1, ct)

  def remove(): (Option[T], FQueue[T]) = if (isEmpty) None -> FQueue.empty[T] else {
    (out: @unchecked) match {
      case x :: Nil =>
        Some(x) -> new FQueue[T](List.empty[T], in.reverse, queueSize - 1, ct)
      case x :: xs =>
        Some(x) -> new FQueue[T](in, xs, queueSize - 1, ct)
    }
  }

  def peek: Option[T] =
    if (isEmpty) None else Some(out.head)

  def peekSecond: Option[T] = remove() match {
    case (None, _) =>
      None
    case (_, queue) =>
      queue.peek
  }

  def iterator: Iterator[T] = {
    val arr = Array.ofDim[T](size)
    if (!isEmpty) (out ++ in.reverse).copyToArray(arr, 0, size)
    arr.iterator
  }

  def size: Int = queueSize

  def isEmpty: Boolean =
    size == 0

}

object FQueue {

  def empty[T](implicit ct: ClassTag[T]): FQueue[T] =
    new FQueue[T](in = List.empty[T], out = List.empty[T], queueSize = 0, ct)

}
