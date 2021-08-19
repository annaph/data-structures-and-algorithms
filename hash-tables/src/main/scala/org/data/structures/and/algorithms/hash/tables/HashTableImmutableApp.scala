package org.data.structures.and.algorithms.hash.tables

import org.data.structures.and.algorithms.hash.tables.HashTableImmutable.printHashTableInfo

import scala.annotation.tailrec

object HashTableImmutableApp extends App {

  // Create hash table
  println("Creating hash table...")
  val hashTable1 = HashTableImmutable.empty[Int, String]
  printHashTableInfo(hashTable1)

  // Insert 3 elements
  println("Inserting 3 elements...")
  val hashTable2 = hashTable1
    .put(100, "Anna")
    .put(101, "Stacey")
    .put(102, "Nicole")
  printHashTableInfo(hashTable2)

  // Search elements
  println("Searching elements...")
  goSearch(100 :: 101 :: 102 :: Nil, hashTable2)
  printHashTableInfo(hashTable2)

  // Remove 3 elements
  println("Removing 3 elements...")
  val hashTable3 = goRemove(101 :: 200 :: 102 :: Nil, hashTable2)
  printHashTableInfo(hashTable3)

  // Search elements
  println("Searching elements...")
  goSearch(100 :: 101 :: 102 :: Nil, hashTable3)
  printHashTableInfo(hashTable3)

  // Contains elements
  println(s"contains '100' ===> ${hashTable3 contains 100}")
  println(s"contains '101' ===> ${hashTable3 contains 101}")
  println(s"contains '102' ===> ${hashTable3 contains 102}")

  def goSearch(keys: List[Int], hashTable: HashTableImmutable[Int, String]): Unit =
    keys.foreach { key =>
      println(s"'$key' search ===> ${hashTable get key}")
    }

  @tailrec
  def goRemove(keys: List[Int],
               hashTable: HashTableImmutable[Int, String]): HashTableImmutable[Int, String] = keys match {
    case Nil =>
      hashTable
    case key :: xs =>
      hashTable.remove(key) match {
        case (Some(value), newHashTable) =>
          println(s"Removed from hash table ===> ($key, $value)")
          goRemove(xs, newHashTable)
        case (None, _) =>
          println(s"Key '$key' cannot be removed since it does not exist!")
          goRemove(xs, hashTable)
      }
  }

}
