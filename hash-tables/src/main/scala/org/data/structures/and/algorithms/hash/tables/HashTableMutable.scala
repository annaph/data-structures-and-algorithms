package org.data.structures.and.algorithms.hash.tables

trait HashTableMutable[K, V] {

  def get(key: K): Option[V]

  def put(key: K, value: V): Unit

  def remove(key: K): Option[V]

  def contains(key: K): Boolean

  def iterator: Iterator[(K, V)]

  def size: Int

  def isEmpty: Boolean

}

object HashTableMutable {

  def empty[K, V]: HashTableMutable[K, V] =
    new HashTableMutableImpl[K, V](hashCodeRange = 17)

  def printHashTableInfo[K, V](hashTable: HashTableMutable[K, V]): Unit = {
    println(s"Hash table is empty ===> ${hashTable.isEmpty}")
    println(s"Hash table size ===> ${hashTable.size}")
    println(s"Hash table content ===> ${hashTable.iterator.mkString("[", ", ", "]")}")
    println("====================================")
  }

}

class HashTableMutableImpl[K, V](hashCodeRange: Int) extends HashTableMutable[K, V] {

  import HashTableMutableImpl.hashKey

  private val arr = Array.fill(hashCodeRange)(List.empty[(K, V)])

  private var hashTableSize = 0

  override def get(key: K): Option[V] = {
    val (_, list) = arrIndexAndList(key)
    list.find(_._1 == key).map(_._2)
  }

  override def put(key: K, value: V): Unit = {
    val (index, list) = arrIndexAndList(key)
    val entry = list.find(_._1 == key)

    entry match {
      case Some(_) =>
        arr(index) = (key, value) :: list.filter(_._1 != key)
      case None =>
        arr(index) = (key, value) :: list
        hashTableSize += 1
    }
  }

  override def remove(key: K): Option[V] = {
    val (index, list) = arrIndexAndList(key)
    val entry = list.find(_._1 == key)

    entry.foreach { _ =>
      arr(index) = list.filter(_._1 != key)
      hashTableSize -= 1
    }

    entry.map(_._2)
  }

  override def contains(key: K): Boolean = {
    val (_, list) = arrIndexAndList(key)
    list.exists(_._1 == key)
  }

  override def iterator: Iterator[(K, V)] = {
    val a = Array.ofDim[(K, V)](size)
    if (!isEmpty) arr.foldLeft(List.empty[(K, V)])(_ ++ _).copyToArray(a, 0, size)
    a.iterator
  }

  override def size: Int =
    hashTableSize

  override def isEmpty: Boolean =
    size == 0

  private def arrIndexAndList(key: K): (Int, List[(K, V)]) = {
    val index = hashKey(key, hashCodeRange)
    val list = arr(index)
    index -> list
  }

}

object HashTableMutableImpl {

  private def hashKey[K](key: K, hashCodeRange: Int): Int = {
    val hash = key.hashCode() % hashCodeRange
    if (hash >= 0) hash else hash + hashCodeRange
  }

}
