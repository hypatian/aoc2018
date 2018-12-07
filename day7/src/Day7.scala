package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.immutable.SortedSet
import scala.collection.mutable.PriorityQueue

object Day7 extends AoCBase(7) {

  val stepPat =
    """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r
  val data = rawData.map({ case stepPat(a, b) => (a(0), b(0)) }).toSeq

  val deps = data.groupBy(_._2).mapValues(_.map(_._1))

  val steps = SortedSet.empty[Char] ++ ('A' to 'Z')

  def problem1() = {
    var done = Set.empty[Char]
    def remaining = steps -- done
    def ready = for {
      s <- remaining
      if (deps.get(s).getOrElse(Seq.empty).toSet -- done).isEmpty
    } yield s
    while ( remaining.nonEmpty ) {
      val next = ready.firstKey
      done += next
      print(next)
    }
  }

  def problem2() = {
    var done = Set.empty[Char]
    var working = PriorityQueue.empty(Ordering[(Int, Char)].reverse)
    def remaining = steps -- done -- working.map(_._2)
    def ready = for {
      s <- remaining
      if (deps.get(s).getOrElse(Seq.empty).toSet -- done).isEmpty
    } yield s
    var maxT = 0
    working ++= (steps -- deps.keys).map(s => (61 + s - 'A', s)).take(5)
    while ( working.nonEmpty ) {
      val (t, s) = working.dequeue()
      done += s
      maxT = t
      working ++= ready.map(s => (t + 61 + s - 'A', s)).take(5 - working.size)
    }
    print(maxT)
  }

}
