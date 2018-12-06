package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day6 extends AoCBase(6) {

  val data: Seq[(Int, Int)] = for {
    Array(x, y) <- rawData.toSeq.map(_.split(", ").map(_.toInt))
  } yield (x, y)

  val width = data.map((x, y) => x).max + 1
  val height = data.map((x, y) => y).max + 1

  def dist(x1: Int, x2: Int, y1: Int, y2: Int) = (x1 - x2).abs + (y1 - y2).abs

  def problem1() = {
    val field = IndexedSeq.tabulate(width + 1, height + 1) { (x, y) =>
      val dists = data.map((px, py) => (dist(px, x, py, y)))
      val minDist = dists.min
      dists.zipWithIndex.
        filter((d, id) => d == minDist).map((d, id) => id) match {
          case Seq(id) => id              // Single nearest index
          case _ => -1
        }
    }
    val finiteIds = (
      (0 until data.size).toSet --
      field.map(_(0)) --                // slice y = 0
      field.map(_(height)) --           // slice y = height
      field(0) --                       // slice x = 0
      field(width)                      // slice x = width
    )
    val areas =
      field.flatten.filter(finiteIds(_)).groupBy(identity).values.map(_.size)
    print(areas.max)
  }

  def problem2() = {
    val field = IndexedSeq.tabulate(width, height) { (x, y) =>
      data.map((px, py) => dist(px, x, py, y)).sum < 10000
    }
    print(field.flatten.count(identity))
  }

}
