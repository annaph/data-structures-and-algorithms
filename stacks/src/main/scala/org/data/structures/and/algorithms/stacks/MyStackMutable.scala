package org.data.structures.and.algorithms.stacks

import scala.reflect.ClassTag

class MyStackMutable[T](maxSize: Int, implicit val ct: ClassTag[T]) {

  private val stack = Array.ofDim[T](maxSize)

  private var top = -1

  def push(t: T): Either[Throwable, Unit] = if (isFull) Left(new Exception("Stack is full!")) else {
    top += 1
    stack(top) = t
    Right(())
  }

  def pop(): Either[Throwable, T] = if (isEmpty) Left(new Exception("Stack is empty!")) else {
    val t = stack(top)
    top -= 1
    Right(t)
  }

  def peek: Option[T] =
    if (isEmpty) None else Some(stack(top))

  def iterator: Iterator[T] = {
    val arr = Array.ofDim[T](size)
    if (size > 0) stack.copyToArray(arr, 0, size)
    arr.iterator
  }

  def size: Int =
    top + 1

  def isEmpty: Boolean =
    top == -1

  def isFull: Boolean =
    top == maxSize - 1

}

object MyStackMutable {

  def apply[T](maxSize: Int = 3)(implicit ct: ClassTag[T]) =
    new MyStackMutable[T](maxSize, ct)

}
