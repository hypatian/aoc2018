package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.annotation.tailrec
import scala.collection.mutable.{Buffer, IndexedSeq}

object Day10 extends AoCBase(10) {

  case class Point(x: Int, y: Int) {
    def +(other: Point) = Point(x + other.x, y + other.y)
    def <=(other: Point) = x <= other.x && y <= other.y
  }

  val pat =
    """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+),\s*(-?\d+)>""".r
  val data = rawData.toSeq.map({
    case pat(px, py, vx, vy) =>
      (Point(px.toInt, py.toInt), Point(vx.toInt, vy.toInt))
  })

  val initPositions = data.map((p, v) => p).toIndexedSeq
  val velocities = data.map((p, v) => v).toIndexedSeq

  def range(ps: Seq[Point]): Point =
    Point(ps.map(_.x).max - ps.map(_.x).min, ps.map(_.y).max - ps.map(_.y).min)

  def plot(ps: Seq[Point]) = {
    val minX = ps.map(_.x).min
    val maxX = ps.map(_.x).max
    val minY = ps.map(_.y).min
    val maxY = ps.map(_.y).max
    println()
    for ( y <- minY to maxY ) {
      for ( x <- minX to maxX ) {
        if ( ps contains Point(x, y) ) print("#") else print(".")
      }
      println()
    }
  }

  var seconds = -1

  def problem1() = {
    var points = initPositions
    var oldPoints = points
    while ( range(points) <= range(oldPoints) ) {
      seconds += 1
      oldPoints = points
      points = points.zip(velocities).map((p, v) => p + v)
    }
    plot(oldPoints)
  }

  def problem2() = print(seconds)

}
