package org.data.structures.and.algorithms.sorting

import org.scalameter._

import scala.util.Random

object SortPerformance extends App {

  listSizeOne1000()
  listSizeTen1000()
  listSizeHundred1000()

  private def listSizeOne1000(): Unit =
    measureSortTimes(
      numbers = generateList(size = 1000, min = -500, max = 1000),
      msg = "1,000 elements list"
    )

  private def listSizeTen1000(): Unit =
    measureSortTimes(
      numbers = generateList(size = 10000, min = -5000, max = 10000),
      msg = "10,000 elements list",
      skipBubble = true,
      skipMerge = true,
    )

  private def listSizeHundred1000(): Unit =
    measureSortTimes(
      numbers = generateList(size = 100000, min = -50000, max = 10000),
      msg = "100,000 elements list",
      skipBubble = true,
      skipInsertion = true,
      skipMerge = true
    )

  private def measureSortTimes(numbers: List[Int],
                               msg: String,
                               skipBubble: Boolean = false,
                               skipInsertion: Boolean = false,
                               skipMerge: Boolean = false): Unit = {
    println(s"====================== $msg =============================")
    println(" ")

    val bubbleSortTime = if (skipBubble) None else Some(measureTime(BubbleSort.sort(numbers)))
    println(f"===> Bubble sort time: " +
      f"${bubbleSortTime.map(time => f"${time.value}%1.2f ${time.units}") getOrElse "==> STACK OVERFLOW <=="}")

    val insertionSortTime = if (skipInsertion) None else Some(measureTime(InsertionSort.sort(numbers)))
    println(f"===> Insertion sort time: " +
      f"${insertionSortTime.map(time => f"${time.value}%1.2f ${time.units}") getOrElse "==> STACK OVERFLOW <=="}")

    val mergeSortTime = if (skipMerge) None else Some(measureTime(MergeSort.sort(numbers)))
    println(f"===> Merge sort time: " +
      f"${mergeSortTime.map(time => f"${time.value}%1.2f ${time.units}") getOrElse "==> STACK OVERFLOW <=="}")

    val quickSortTime = measureTime(QuickSort.sort(numbers))
    println(f"===> Quick sort time: ${quickSortTime.value}%1.2f ${quickSortTime.units}")

    val standardSortTime = measureTime(numbers.sorted)
    println(f"===> Standard sort time: ${standardSortTime.value}%1.2f ${standardSortTime.units}")

    println(" ")
    println("========================================================================")
  }

  private def measureTime(action: => Unit): Quantity[Double] = {
    val configParameters = Seq(
      KeyValue(Key.exec.minWarmupRuns -> 12),
      KeyValue(Key.exec.maxWarmupRuns -> 51),
      KeyValue(Key.exec.benchRuns -> 37),
      KeyValue(Key.verbose -> false)
    )

    config(configParameters: _*)
      .withWarmer(new Warmer.Default)
      .measure(action)
  }

  private def generateList(size: Int, min: Int, max: Int): List[Int] = {
    val random = new Random()
    val result = for (_ <- 1 to size) yield random.between(min, max)
    result.toList
  }

}
