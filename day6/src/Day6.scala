package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day6 extends AoCBase(6) {

  val data: Seq[(Int, Int, Int)] = for {
    (Array(x, y), id) <-
      rawData.toSeq.map(_.split(", ").map(_.toInt)).zipWithIndex
  } yield (id, x, y)

  val maxX = data.map((id, x, y) => x).max
  val maxY = data.map((id, x, y) => y).max

  def dist(x1: Int, x2: Int, y1: Int, y2: Int) = (x1 - x2).abs + (y1 - y2).abs

  def problem1() = {
    val field = IndexedSeq.tabulate(maxX + 2, maxY + 2) { (x, y) =>
      val distIds = data.map((id, px, py) => (dist(px, x, py, y), id))
      val minDist = distIds.map((d, id) => d).min
      val minPoints = distIds.filter((d, id) => d == minDist).map((d, id) => id)
      if ( minPoints.size > 1 ) -1 else minPoints(0)
    }
    val finiteIds = (
      data.map((id, x, y) => id).toSet --
      field.map(_(0)) --
      field.map(_(maxY + 1)) --
      field(0) --
      field(maxX + 1)
    )
    val areas =
      field.flatten.filter(finiteIds(_)).groupBy(identity).
        mapValues(_.count(_ => true)).map((id, area) => area)
    print(areas.max)
  }

  def problem2() = {
    val field = IndexedSeq.tabulate(maxX + 2, maxY + 2) { (x, y) =>
      data.map((_, px, py) => dist(px, x, py, y)).sum < 10000
    }
    print(field.flatten.count(identity))
  }

}
