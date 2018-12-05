package org.hypatian.demo.aoc

import scala.io.Source

trait AoCBase(val day: Int) {

  var filename: String = s"day${day}/input.txt"
  def rawData: Iterator[String] = Source.fromFile(filename).getLines
  def problem1(): Unit
  def problem2(): Unit

  def main(args: Array[String]) = {

    def time[T](body: => T): T = {
      val t0 = System.currentTimeMillis
      val r = body
      val t1 = System.currentTimeMillis
      println(s" (in ${t1 - t0}ms)")
      r
    }

    if ( args.length == 1 ) filename = args(0)
    println(s"Day ${day}:")
    print(s"  Problem 1 solution: ")
    time(problem1())
    print(s"  Problem 2 solution: ")
    time(problem2())
  }

}
