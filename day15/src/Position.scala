package org.hypatian.demo.aoc2018

case class Position(x: Int, y: Int) {
  def adjacent: Iterator[Position] =
    Iterator(copy(y = y - 1), copy(x = x - 1), copy(x = x + 1), copy(y = y + 1))
}
object Position {
  implicit val positionOrdering: Ordering[Position] =
    Ordering.by(p => (p.y, p.x))
}
