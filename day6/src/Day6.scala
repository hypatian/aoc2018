package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day6 extends AoCBase(6) {

  val data: Seq[(Int, Int)] = for {
    Array(x, y) <- rawData.toSeq.map(_.split(", ").map(_.toInt))
  } yield (x, y)

  val width = data.map((x, y) => x).max + 1
  val height = data.map((x, y) => y).max + 1

  def dist(x1: Int, x2: Int, y1: Int, y2: Int) = (x1 - x2).abs + (y1 - y2).abs

  implicit class IndexedSeq2DSlices[T](s: IndexedSeq[IndexedSeq[T]]) {
    def col(x: Int): IndexedSeq[T] = s(x)
    def row(y: Int): IndexedSeq[T] = s.map(_(y))
  }

  def problem1() = {
    val field = IndexedSeq.tabulate(width + 1, height + 1) { (x, y) =>
      data.map((px, py) => (dist(px, x, py, y))).zipWithIndex.sorted match {
        case Seq((d1, _), (d2, _), _: _*) if d1 == d2 => -1
        case Seq((_, id), _: _*) => id
      }
    }
    val finiteIds =
      (data.indices.toSet --
        field.row(0) -- field.row(height) --
        field.col(0) -- field.col(width))
    val areas =
      field.flatten.filter(finiteIds).groupBy(identity).values.map(_.size)
    print(areas.max)
  }

  def problem2() = {
    val field = IndexedSeq.tabulate(width, height) { (x, y) =>
      data.map((px, py) => dist(px, x, py, y)).sum < 10000
    }
    print(field.flatten.count(identity))
  }

}
