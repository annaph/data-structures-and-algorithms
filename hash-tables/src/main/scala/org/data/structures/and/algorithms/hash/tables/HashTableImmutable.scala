package org.data.structures.and.algorithms.hash.tables

trait HashTableImmutable[K, V] {

  def get(key: K): Option[V]

  def put(key: K, value: V): HashTableImmutable[K, V]

  def remove(key: K): (Option[V], HashTableImmutable[K, V])

  def contains(key: K): Boolean

  def iterator: Iterator[(K, V)]

  def size: Int

  def isEmpty: Boolean

}

object HashTableImmutable {

  type Entry[K, V] = (K, V)

  def empty[K, V]: HashTableImmutable[K, V] =
    HashTableImmutableImpl[K, V]()

  def printHashTableInfo[K, V](hashTable: HashTableImmutable[K, V]): Unit = {
    println(s"Hash table is empty ===> ${hashTable.isEmpty}")
    println(s"Hash table size ===> ${hashTable.size}")
    println(s"Hash table content ===> ${hashTable.iterator.mkString("[", ", ", "]")}")
    println("====================================")
  }

}

class HashTableImmutableImpl[K, V] private(hashVector: Vector[List[(K, V)]],
                                           hashTableSize: Int) extends HashTableImmutable[K, V] {

  import HashTableImmutableImpl.hashKey

  override def get(key: K): Option[V] = {
    val (_, list) = arrIndexAndList(key)
    list.find(_._1 == key).map(_._2)
  }

  override def put(key: K, value: V): HashTableImmutable[K, V] = {
    val (index, list) = arrIndexAndList(key)
    val entry = list.find(_._1 == key)

    entry match {
      case Some(_) =>
        val newList = (key, value) :: list.filter(_._1 != key)
        new HashTableImmutableImpl(hashVector.updated(index, newList), hashTableSize)
      case None =>
        val newList = (key, value) :: list
        new HashTableImmutableImpl(hashVector.updated(index, newList), hashTableSize + 1)
    }
  }

  override def remove(key: K): (Option[V], HashTableImmutable[K, V]) = {
    val (index, list) = arrIndexAndList(key)
    val entry = list.find(_._1 == key)

    entry match {
      case Some((_, value)) =>
        val newList = list.filter(_._1 != key)
        Some(value) -> new HashTableImmutableImpl(hashVector.updated(index, newList), hashTableSize - 1)
      case None =>
        None -> this
    }
  }

  override def contains(key: K): Boolean = {
    val (_, list) = arrIndexAndList(key)
    list.exists(_._1 == key)
  }

  override def iterator: Iterator[(K, V)] = {
    val arr = Array.ofDim[(K, V)](size)
    if (!isEmpty) hashVector.foldLeft(List.empty[(K, V)])(_ ++ _).copyToArray(arr, 0, size)
    arr.iterator
  }

  override def size: Int =
    hashTableSize

  override def isEmpty: Boolean =
    hashTableSize == 0

  private def arrIndexAndList(key: K): (Int, List[(K, V)]) = {
    val index = hashKey(key, hashVector.size)
    val list = hashVector(index)
    index -> list
  }

}

object HashTableImmutableImpl {

  def apply[K, V](hashCodeRange: Int = 17): HashTableImmutableImpl[K, V] =
    new HashTableImmutableImpl[K, V](
      hashVector = Vector.fill(hashCodeRange)(List.empty[(K, V)]),
      hashTableSize = 0
    )

  private def hashKey[K](key: K, hashCodeRange: Int): Int = {
    val hash = key.hashCode() % hashCodeRange
    if (hash >= 0) hash else hash + hashCodeRange
  }

}
