package org.data.structures.and.algorithms.stacks

import scala.reflect.ClassTag

class MyStackImmutable[T] private(stack: List[T], stackSize: Int, maxSize: Int, implicit val ct: ClassTag[T]) {

  def push(t: T): Either[Throwable, MyStackImmutable[T]] =
    if (isFull) Left(new Exception("Stack is full!")) else Right {
      new MyStackImmutable[T](t :: stack, stackSize + 1, maxSize, ct)
    }

  def pop(): Either[Throwable, (T, MyStackImmutable[T])] =
    if (isEmpty) Left(new Exception("Stack is empty!")) else Right {
      val t = stack.head
      t -> new MyStackImmutable(stack.tail, stackSize - 1, maxSize, ct)
    }

  def peek: Option[T] =
    if (isEmpty) None else Some(stack.head)

  def iterator: Iterator[T] = {
    val arr = Array.ofDim[T](size)
    if (size > 0) stack.reverse.copyToArray(arr, 0, size)
    arr.iterator
  }

  def size: Int = stackSize

  def isEmpty: Boolean =
    stack.isEmpty

  def isFull: Boolean =
    stackSize == maxSize

}

object MyStackImmutable {

  def apply[T](maxSize: Int = 3)(implicit ct: ClassTag[T]): MyStackImmutable[T] =
    new MyStackImmutable[T](List.empty[T], stackSize = 0, maxSize, ct)

}