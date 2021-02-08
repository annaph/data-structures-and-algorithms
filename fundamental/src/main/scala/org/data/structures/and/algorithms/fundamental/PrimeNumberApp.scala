package org.data.structures.and.algorithms.fundamental

import scala.math.sqrt

object PrimeNumberApp extends App {

  val primes: LazyList[Int] = 2 #:: LazyList.from(3).filter { x =>
    !primes.takeWhile(_ <= sqrt(x).toInt).exists(y => x % y == 0)
  }

  val result = primes take 12
  println(result.toList)

}
