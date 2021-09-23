package org.data.structures.and.algorithms.binary.tress

import org.data.structures.and.algorithms.binary.tress.BinaryTree.{DictionaryOps, dictionary, printInfo}

object DictionaryApp extends App {

  // Insert words
  val myDictionary = dictionary(("cat", 5), ("dog", 7), ("the", 12), ("for", 4), ("then", 11))
  printInfo(myDictionary)
  assert(myDictionary.insert("cat", 7).isLeft, "'cat' already present")

  // Search words
  println(s"'cat' exists: ${myDictionary get "cat"}")
  println(s"'for' exists: ${myDictionary get "for"}")
  println(s"'anna' exists: ${myDictionary get "anna"}")

  // Update value
  val newMyDictionary = myDictionary.update("cat", 101) getOrElse myDictionary
  assert(newMyDictionary.get("cat") contains 101, "Value for word 'cat' should be updated!")
  assert(newMyDictionary.update("anna", 1).isLeft, "Value for word 'anna' cannot updated!")
  printInfo(newMyDictionary)

}
