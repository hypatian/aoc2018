package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.annotation.tailrec
import scala.collection.mutable.{Buffer, IndexedSeq}
import scala.math.{max, min}

object Day10 extends AoCBase(10) {

  case class Point(x: Int, y: Int) {
    def +(other: Point) = Point(x + other.x, y + other.y)
    def *(scale: Int) = Point(x * scale, y * scale)
  }
  object Point {
    val MaxValue: Point = Point(Int.MaxValue, Int.MaxValue)
    val MinValue: Point = Point(Int.MinValue, Int.MinValue)
  }

  implicit class PointSeqOps(self: Seq[Point]) {
    def height: Int = self.map(_.y).max - self.map(_.y).min
    def range: (Point, Point) =
      self.foldLeft((Point.MaxValue, Point.MinValue))({
        case ((a, b), p) =>
          (Point(min(a.x, p.x), min(a.y, p.y)),
            Point(max(b.x, p.x), max(b.y, p.y)))
      })
    def plot: String = {
      val (Point(minX, minY), Point(maxX, maxY)) = range
      (minY to maxY).map({ y =>
        (minX to maxX).map({ x =>
          if ( self contains Point(x, y) ) '#' else '.' }).mkString
      }).mkString("\n")
    }
  }

  val pat =
    """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+),\s*(-?\d+)>""".r
  val data = rawData.toSeq.map({
    case pat(px, py, vx, vy) =>
      (Point(px.toInt, py.toInt), Point(vx.toInt, vy.toInt))
  })

  val p0 = data.map((p, v) => p).toIndexedSeq
  val v = data.map((p, v) => v).toIndexedSeq

  def problem1() = {
    val Some(tGoal, pGoal) = Stream.from(0).map({ t =>
      val pt = p0.zip(v.map(_ * t)).map(_ + _)
      (t, pt.height, pt)
    }).sliding(2).collectFirst({
      case Stream((t1, h1, pt1), (t2, h2, pt2)) if h2 > h1 => (t1, pt1)
    })
    println()
    println(pGoal.plot)
    println(s"In $tGoal seconds")
  }

  def problem2() = () // Done by problem1

}
