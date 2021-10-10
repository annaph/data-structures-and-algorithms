package org.data.structures.and.algorithms.sorting

object BubbleSort {

  def sort[T](list: List[T])(implicit or: Ordering[T]): List[T] = list match {
    case Nil =>
      Nil
    case x :: Nil =>
      List(x)
    case _ =>
      val (target, rest) = targetAndRest(list)
      target :: sort(rest)
  }

  private def targetAndRest[T](list: List[T])(implicit or: Ordering[T]): (T, List[T]) =
    (list: @unchecked) match {
      case x1 :: x2 :: Nil if or.gt(x1, x2) =>
        x2 -> List(x1)
      case x1 :: x2 :: Nil =>
        x1 -> List(x2)
      case x :: xs =>
        val (target, rest) = targetAndRest(xs)
        if (or.gt(x, target)) target -> (x :: rest) else x -> (target :: rest)
    }

}
