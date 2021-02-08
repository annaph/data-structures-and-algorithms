package org.data.structures.and.algorithms.fundamental

object GardenNettingGreedyAlgoApp extends App {

  val perimeter = 50
  val netLengths = Array(3, 1, 15, 10, 2)
  val result = selectNets(perimeter, netLengths)

  println(result)

  def selectNets(perimeter: Int, netLengths: Array[Int]): List[Int] = {
    val sortedNetLengths = netLengths.sorted(Ordering.Int.reverse)

    val selection = sortedNetLengths.foldLeft(Selection(distance = perimeter)) {
      case (Selection(distance, lengths), netLength) =>
        val n = distance / netLength
        val newDistance = distance - (n * netLength)
        val newLengths = List.fill(n)(netLength) ++ lengths

        Selection(newDistance, newLengths)
    }

    selection.lengths
  }

  case class Selection(distance: Int, lengths: List[Int] = List.empty)

}
