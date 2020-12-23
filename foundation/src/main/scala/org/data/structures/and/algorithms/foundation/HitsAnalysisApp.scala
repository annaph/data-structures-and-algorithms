package org.data.structures.and.algorithms.foundation

import scala.collection.mutable

/**
 * Result should be:
 *
 * (com,1020)
 * (stackoverflow.com,10)
 * (sports.yahoo.com,50)
 * (google.com,900)
 * (sports,1)
 * (org,3)
 * (wikipedia.org,3)
 * (mobile.sports,1)
 * (es.wikipedia.org,1)
 * (mail.yahoo.com,60)
 * (en.wikipedia.org,2)
 * (mobile.sports.yahoo.com,10)
 * (yahoo.com,110)
 *
 */
object HitsAnalysisApp extends App {

  val input = Array(
    (900, "google.com"),
    (60, "mail.yahoo.com"),
    (10, "mobile.sports.yahoo.com"),
    (40, "sports.yahoo.com"),
    (10, "stackoverflow.com"),
    (2, "en.wikipedia.org"),
    (1, "es.wikipedia.org"),
    (1, "mobile.sports"))

  val result = analyse(input)
  result foreach println

  def analyse(input: Array[(Int, String)]): Array[(Int, String)] =
    toArray(hits(input))

  private def hits(input: Array[(Int, String)]): Map[String, Int] = {
    def update(map: mutable.Map[String, Int], subDomain: String, count: Int): Unit = {
      map.get(subDomain) match {
        case Some(subDomainCount) =>
          map.update(subDomain, subDomainCount + count)
        case None =>
          map += (subDomain -> count)
      }
    }

    val result = input.foldLeft(mutable.Map.empty[String, Int]) {
      case (acc, (count, domain)) =>
        HitsAnalysisApp.subDomains(domain).foreach(subDomain => update(acc, subDomain, count))
        acc
    }

    result.toMap
  }

  private def subDomains(domain: String): List[String] =
    domain.split("\\.").toList.reverse.foldLeft(List.empty[String]) {
      case (Nil, subDomain) =>
        subDomain :: Nil
      case (x :: xs, subDomain) =>
        s"$subDomain.$x" :: x :: xs
    }

  private def toArray(map: Map[String, Int]): Array[(Int, String)] = map.toArray.map {
    case (str, n) =>
      n -> str
  }

}
