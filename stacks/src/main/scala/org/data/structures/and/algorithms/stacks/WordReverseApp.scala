package org.data.structures.and.algorithms.stacks

import scala.annotation.tailrec
import scala.io.StdIn

object WordReverseApp extends App {

  print("Enter a word: ")
  val word = StdIn.readLine()

  println(s"You entered: $word")
  println(s"Reverse word: ${reverse(word)}")

  def reverse(word: String): String = {
    val stack = MyStackMutable[Char](maxSize = word.length)
    for (ch <- word) stack push ch

    @tailrec
    def go(acc: StringBuilder): StringBuilder =
      if (stack.isEmpty) acc else {
        acc append stack.pop().toOption.get
        go(acc)
      }

    go(new StringBuilder).toString()
  }

}
