package org.data.structures.and.algorithms.hash.tables

import org.data.structures.and.algorithms.hash.tables.HashTableMutable.printHashTableInfo

object HashTableMutableApp extends App {

  // Create hash table
  println("Creating hash table...")
  val hashTable = HashTableMutable.empty[Int, String]
  printHashTableInfo(hashTable)

  // Insert 3 elements
  println("Inserting 3 elements...")
  goInsert((100, "Anna") :: (101, "Stacey") :: (102, "Nicole") :: (100, "Anna") :: Nil)
  printHashTableInfo(hashTable)

  // Search elements
  println("Searching elements...")
  goSearch(100 :: 101 :: 102 :: Nil)
  printHashTableInfo(hashTable)

  // Remove 3 elements
  println("Removing 3 elements...")
  goRemove(101 :: 200 :: 102 :: Nil)
  printHashTableInfo(hashTable)

  // Search elements
  println("Searching elements...")
  goSearch(100 :: 101 :: 102 :: Nil)
  printHashTableInfo(hashTable)

  // Contains elements
  println(s"contains '100' ===> ${hashTable contains 100}")
  println(s"contains '101' ===> ${hashTable contains 101}")
  println(s"contains '102' ===> ${hashTable contains 102}")

  def goInsert(elements: List[(Int, String)]): Unit =
    elements.foreach {
      case (key, value) =>
        hashTable.put(key, value)
    }

  def goSearch(keys: List[Int]): Unit =
    keys.foreach { key =>
      println(s"'$key' search ===> ${hashTable get key}")
    }

  def goRemove(keys: List[Int]): Unit =
    keys.foreach { key =>
      hashTable.remove(key) match {
        case Some(value) =>
          println(s"Removed from hash table ===> ($key, $value)")
        case None =>
          println(s"Key '$key' cannot be removed since it does not exist!")
      }
    }

}
