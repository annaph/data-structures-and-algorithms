package org.data.structures.and.algorithms.lists

object WordCountByAlphabetApp extends App {

  val result = solution(MiniDictionary.myDict)

  result.foreach {
    case (letter, count) =>
      println(s"'$letter' -> $count")

  }

  def solution(dict: List[String]): List[(Char, Int)] = {
    dict
      .groupBy(_.head)
      .map(entry => entry._1 -> entry._2.size)
      .toList
      .sortWith {
        case ((letter1, _), (letter2, _)) =>
          letter1 < letter2
      }
  }

}
