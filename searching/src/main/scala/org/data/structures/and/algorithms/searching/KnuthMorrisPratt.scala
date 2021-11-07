package org.data.structures.and.algorithms.searching

import scala.annotation.tailrec
import scala.util.Try

object KnuthMorrisPratt {

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
    val textSize = text.length
    val patternSize = pattern.length

    if (textSize == 0 || patternSize == 0 || start >= textSize) None else {
      val table = prefixTable(pattern)

      @tailrec
      def updatePatternIndex(patternIndex: Int, ch: Char): Int =
        if (patternIndex == -1) 0
        else if (ch == pattern(patternIndex)) patternIndex + 1
        else updatePatternIndex(Try(table(patternIndex - 1)) getOrElse -1, ch)

      @tailrec
      def go(patternIndex: Int = 0, textIndex: Int): Option[Int] = {
        if (patternIndex == patternSize) Some(textIndex - patternSize)
        else if (textIndex == textSize) None
        else go(updatePatternIndex(patternIndex, text(textIndex)), textIndex + 1)
      }

      go(textIndex = start)
    }
  }

  def prefixTable(text: String): Vector[Int] = {
    val chars: Array[Char] = text.toCharArray

    @tailrec
    def go(table: Vector[Int], ch: Char, k: Int): Int = k match {
      case -1 =>
        0
      case _ if ch == chars(k) =>
        k + 1
      case _ =>
        val newK = Try(table(k - 1)).recover(_ => -1).get
        go(table, ch, newK)
    }

    chars.zipWithIndex.tail.foldLeft(Vector(0)) {
      case (table, (ch, index)) =>
        table :+ go(table, ch, table(index - 1))
    }
  }

}
