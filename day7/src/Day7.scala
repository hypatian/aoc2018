package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.immutable.SortedSet
import scala.collection.mutable.PriorityQueue

object Day7 extends AoCBase(7) {

  val stepPat =
    """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  val deps =
    rawData.map({ case stepPat(pre, post) => (pre(0), post(0)) }).toSeq
      .groupBy((pre, post) => post).mapValues(_.map((pre, post) => pre).toSet)

  val steps = SortedSet.empty[Char] ++ ('A' to 'Z')

  def problem1() = {
    var done = Set.empty[Char]
    def remaining = steps -- done
    def isReady(s: Char) = (deps.getOrElse(s, Set.empty) -- done).isEmpty
    def ready = remaining.filter(isReady)
    while ( remaining.nonEmpty ) {
      val next = ready.firstKey
      done += next
      print(next)
    }
  }

  def problem2() = {
    var done = Set.empty[Char]
    var working = PriorityQueue.empty(Ordering[(Int, Char)].reverse)
    def remaining = steps -- done -- working.map((t, s) => s)
    def isReady(s: Char) = (deps.getOrElse(s, Set.empty) -- done).isEmpty
    def ready = remaining.filter(isReady)
    working ++= (steps -- deps.keys).map(s => (61 + s - 'A', s)).take(5)
    var currentT = 0
    while ( working.nonEmpty ) {
      val (t, s) = working.dequeue()
      done += s
      working ++= ready.map(s => (t + 61 + s - 'A', s)).take(5 - working.size)
      currentT = t
    }
    print(currentT)
  }

}
