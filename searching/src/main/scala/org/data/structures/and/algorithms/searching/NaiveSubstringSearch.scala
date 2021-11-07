package org.data.structures.and.algorithms.searching

import scala.annotation.tailrec

object NaiveSubstringSearch {

  def findAll(pattern: String, text: String): Seq[Int] = {
    @tailrec
    def go(start: Int = 0, acc: Seq[Int] = Seq.empty[Int]): Seq[Int] =
      findFirst(pattern, text, start) match {
        case Some(i) =>
          go(i + pattern.length, acc :+ i)
        case None =>
          acc
      }

    go()
  }

  def findFirst(pattern: String, text: String, start: Int = 0): Option[Int] = {
    @tailrec
    def go(i: Int): Option[Int] = i match {
      case _ if i > (text.length - pattern.length) =>
        None
      case _ if pattern.indices.forall(j => pattern(j) == text(j + i)) =>
        Some(i)
      case _ =>
        go(i + 1)
    }

    if (start < text.length) go(start) else None
  }

}
