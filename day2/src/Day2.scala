package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day2 extends AoCBase(2) {

  def data: Stream[String] = rawData.toStream

  def problem1() = {
    implicit class StringHasNDups(self: String) {
      def hasExactlyNDuplicates(n: Int): Boolean =
        self.groupBy(identity).exists((_, v) => v.size == n)
    }
    val has2Count = data.count(_.hasExactlyNDuplicates(2))
    val has3Count = data.count(_.hasExactlyNDuplicates(3))
    println(has2Count * has3Count)
  }

  def problem2() = {
    implicit class StringDropIndex(self: String) {
      def dropIndex(n: Int): String = self.take(n) ++ self.drop(n + 1)
    }
    val idsDropOne = for {
      id <- data
      idDropOne <- (0 to id.size).map(i => id.dropIndex(i)).distinct
    } yield idDropOne
    val matches =
      idsDropOne.groupBy(identity).filter((_, v) => v.size == 2).keySet

    assert(matches.size == 1)
    println(matches.head)
  }

}
