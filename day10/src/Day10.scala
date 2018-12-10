package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.annotation.tailrec
import scala.collection.mutable.{Buffer, IndexedSeq}

object Day10 extends AoCBase(10) {

  case class Point(x: Int, y: Int) {
    def +(other: Point) = Point(x + other.x, y + other.y)
    def <(other: Point) = x < other.x && y < other.y
  }

  val pat =
    """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+),\s*(-?\d+)>""".r
  val data = rawData.toSeq.map({
    case pat(px, py, vx, vy) =>
      (Point(px.toInt, py.toInt), Point(vx.toInt, vy.toInt))
  })

  val initPositions = data.map((p, v) => p)
  val velocities = data.map((p, v) => v)

  def range(ps: Seq[Point]): Point =
    Point(ps.map(_.x).max - ps.map(_.x).min, ps.map(_.y).max - ps.map(_.y).min)

  def plot(ps: Seq[Point]) = {
    val minX = ps.map(_.x).min
    val maxX = ps.map(_.x).max
    val minY = ps.map(_.y).min
    val maxY = ps.map(_.y).max
    val pss = ps.toSet
    for ( y <- minY to maxY ) {
      for ( x <- minX to maxX ) {
        if ( pss(Point(x, y)) ) print("#") else print(".")
      }
      println()
    }
  }

  var seconds = 0

  def problem1() = {
    var ps = initPositions
    println()
    while ( true ) {
      val newPs = ps.zip(velocities).map((p, v) => p + v)
      if ( range(ps) < range(newPs) ) {
        plot(ps)
        return
      }
      ps = newPs
      seconds += 1
    }
  }

  def problem2() = print(seconds)

}
