package org.data.structures.and.algorithms

package object sorting {

  object AscendingIntOrder extends Ordering[Int] {
    override def compare(x: Int, y: Int): Int =
      if (x < y) -1 else if (x > y) 1 else 0
  }

  object DescendingIntOrder extends Ordering[Int] {
    override def compare(x: Int, y: Int): Int =
      if (x > y) -1 else if (x < y) 1 else 0
  }

  object AscendingCharOrder extends Ordering[Char] {
    override def compare(x: Char, y: Char): Int =
      if (x < y) -1 else if (x > y) 1 else 0
  }

  object DescendingCharOrder extends Ordering[Char] {
    override def compare(x: Char, y: Char): Int =
      if (x > y) -1 else if (x < y) 1 else 0
  }

  object AscendingStringOrder extends Ordering[String] {
    override def compare(x: String, y: String): Int =
      x compareTo y
  }

  object DescendingStringOrder extends Ordering[String] {
    override def compare(x: String, y: String): Int =
      -(x compareTo y)
  }

}
