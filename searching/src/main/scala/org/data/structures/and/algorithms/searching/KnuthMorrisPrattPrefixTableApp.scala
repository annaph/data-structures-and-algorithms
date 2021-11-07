package org.data.structures.and.algorithms.searching

import org.data.structures.and.algorithms.searching.KnuthMorrisPratt.prefixTable

object KnuthMorrisPrattPrefixTableApp extends App {

  val pattern1 = "xyxyxzx"
  val prefixTable1 = prefixTable(pattern1)

  assert(
    prefixTable1 == Vector(0, 0, 1, 2, 3, 0, 1),
    message = s"Prefix table for '$pattern1' text should be (0, 0, 1, 2, 3, 0, 1)!"
  )

  val pattern2 = "abcabcd"
  val prefixTable2 = prefixTable(pattern2)

  assert(
    prefixTable2 == Vector(0, 0, 0, 1, 2, 3, 0),
    message = s"Prefix table for '$pattern2' text should be (0, 0, 0, 1, 2, 3, 0)!"
  )

  val pattern3 = "aabaaab"
  val prefixTable3 = prefixTable(pattern3)

  assert(
    prefixTable3 == Vector(0, 1, 0, 1, 2, 2, 3),
    message = s"Prefix table for '$pattern3' text should be (0, 1, 0, 1, 2, 2, 3)!"
  )

}
