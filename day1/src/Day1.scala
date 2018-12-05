package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day1 extends AoCBase(1) {

  def data: Iterator[Int] = rawData.map(_.toInt)

  def problem1() = print(data.sum)

  def problem2() = {
    def deltas = Iterator.continually(data).flatten
    def sums = deltas.scanLeft(0)(_ + _)
    def seen = sums.scanLeft(Set.empty)(_ + _)
    def dups = (seen zip sums).filter(_ contains _).map((_, sum) => sum)
    print(dups.next)
  }

}
