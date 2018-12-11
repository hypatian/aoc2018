package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day11 extends AoCBase(11) {

  val serial: Int = 4842

  def powerLevel(x: Int, y: Int): Int =
    (((x + 10) * y + serial) * (x + 10)) / 100 % 10 - 5

  def gridLevel(x: Int, y: Int, s: Int): Int =
    Seq.tabulate(s, s)((dx, dy) => powerLevel(x + dx, y + dy)).flatten.sum

  def maxLevelAtSize(s: Int): (Int, Int) =
    (for { x <- 1 to (301 - s); y <- 1 to (301 - s) } yield (x, y)).
      maxBy(gridLevel(_, _, s))

  def maxLevel: (Int, Int, Int) =
    Stream.range(1, 300).map({ s =>
      val (x, y) = maxLevelAtSize(s)
      (x, y, s)
    }).sliding(2).collectFirst({
      case Stream((x1, y1, s1), (x2, y2, s2))
          if gridLevel(x2, y2, s2) < gridLevel(x1, y1, s1) =>
        (x1, y1, s1)
    }).get

  def problem1() = {
    print(maxLevelAtSize(3))
  }

  def problem2() = {
    print(maxLevel)
  }

}
