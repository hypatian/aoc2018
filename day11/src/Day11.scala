package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.Map

object Day11 extends AoCBase(11) {

  val serial: Int = 4842

  private[this] val powerLevelCache: Map[(Int, Int, Int), Int] = Map.empty
  def powerLevel(x: Int, y: Int, s: Int): Int =
    powerLevelCache.getOrElseUpdate((x, y, s), s match {
      case 1 => (((x + 10) * y + serial) * (x + 10)) / 100 % 10 - 5
      case 2 =>
        powerLevel(x, y,     1) + powerLevel(x + 1, y,     1) +
        powerLevel(x, y + 1, 1) + powerLevel(x + 1, y + 1, 1)
      case n =>
        powerLevel(x + n - 1, y, 1) + powerLevel(x, y + n - 1, 1) +
        powerLevel(x, y, n - 1)     + powerLevel(x + 1, y + 1, n - 1) -
        powerLevel(x + 1, y + 1, n - 2)
    })

  def problem1() =
    print(
      (for { x <- 1 to 298; y <- 1 to 298 } yield (x, y)).
        maxBy(powerLevel(_, _, 3)))

  def problem2() =
    print(
      (for { s <- 1 to 300; x <- 1 to 301 - s; y <- 1 to 301 - s }
         yield (x,y,s)).
        maxBy(powerLevel))

}
