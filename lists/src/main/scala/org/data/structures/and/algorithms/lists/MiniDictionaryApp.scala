package org.data.structures.and.algorithms.lists

object MiniDictionaryApp extends App {

  import MiniDictionary.myDict

  println("===> My dictionary:")
  println(myDict mkString ", ")

  val sortedMyDict = myDict.sorted

  println("===> My dictionary sorted:")
  println(sortedMyDict mkString ", ")

  val duplicatedWords = myDict.groupBy(identity).collect {
    case (word, List(_, _, _*)) =>
      word
  }

  println("===> Duplicated words:")
  println(duplicatedWords mkString ", ")

  println(s"===> Number of words: ${myDict.size}")
  println(s"===> 'monkey' exists: ${myDict contains "monkey"}")
  println(s"===> 'university' exists: ${myDict contains "university"}")

}

object MiniDictionary {

  val myDict = List(
    "apple", "cow", "color", "god",
    "goat", "dog", "house", "mother", "orange",
    "rat", "zeal", "university", "apple", "monkey",
    "honorificabilitudinitatibus",
    "floccinaucinihilipilification", "zeal",
    "pseudopseudohypoparathyroidism", "rat",
    "supercalifragilisticexpialidocious",
    "pneumonoultramicroscopicsilicovolcanoconiosis"
  )

}
