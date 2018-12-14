package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.{Buffer, IndexedSeq => MIndexedSeq}

object Day13 extends AoCBase(13) {

  enum Direction {
    case Up, Down, Left, Right
    def underlying: Char = this match {
      case Up | Down => '|'
      case Left | Right => '-'
    }
    def turn(n: Int): Direction = (this, n % 3) match {
      case (d, 1) => d
      case (Up, 0) => Left
      case (Up, 2) => Right
      case (Down, 0) => Right
      case (Down, 2) => Left
      case (Left, 0) => Down
      case (Left, 2) => Up
      case (Right, 0) => Up
      case (Right, 2) => Down
    }
  }
  import Direction.{Up, Down, Left, Right}

  class Map(
    val paths: IndexedSeq[IndexedSeq[Char]],
    var carts: IndexedSeq[Cart])
  {
    def tick(remove: Boolean = false): Unit = {
      val orig = carts
      for ( i <- orig.indices ) {
        if ( ! carts(i).removed ) {
          carts = carts.updated(i, orig(i).move(this))
          for {
            (x, y) <- carts.filter(!_.removed).map(c => (c.x, c.y))
            if carts.filter(!_.removed).count(c => (c.x == x && c.y == y)) > 1
          } {
            if ( remove ) {
              carts = carts.map({ c =>
                if ( c.x == x && c.y == y ) c.copy(removed = true) else c
              })
            } else {
              throw Crash(x, y)
            }
          }
        }
      }
      carts = carts.filter(!_.removed).sorted
    }
  }
  object Map {
    def apply(
      paths: IndexedSeq[IndexedSeq[Char]], carts: IndexedSeq[Cart]): Map =
      new Map(paths, carts)
  }

  case class Crash(x: Int, y: Int) extends Exception

  case class Cart(y: Int, x: Int, d: Direction, turns: Int, removed: Boolean = false) {
    def move(map: Map): Cart = d match {
      case Up => map.paths(y - 1)(x) match {
        case '|' => copy(y = y - 1)
        case '/' => copy(y = y - 1, d = Right)
        case '\\' => copy(y = y - 1, d = Left)
        case '+' => copy(y = y - 1, d = d.turn(turns), turns = turns + 1)
      }
      case Down => map.paths(y + 1)(x) match {
        case '|' => copy(y = y + 1)
        case '/' => copy(y = y + 1, d = Left)
        case '\\' => copy(y = y + 1, d = Right)
        case '+' => copy(y = y + 1, d = d.turn(turns), turns = turns + 1)
      }
      case Left => map.paths(y)(x - 1) match {
        case '-' => copy(x = x - 1)
        case '/' => copy(x = x - 1, d = Down)
        case '\\' => copy(x = x - 1, d = Up)
        case '+' => copy(x = x - 1, d = d.turn(turns), turns = turns + 1)
      }
      case Right => map.paths(y)(x + 1) match {
        case '-' => copy(x = x + 1)
        case '/' => copy(x = x + 1, d = Up)
        case '\\' => copy(x = x + 1, d = Down)
        case '+' => copy(x = x + 1, d = d.turn(turns), turns = turns + 1)
      }
    }
  }
  implicit val cartOrdering: Ordering[Cart] = Ordering.by(c => (c.x, c.y))

  def map: Map = {
    val mapData = rawData.toSeq.map(_.to[MIndexedSeq]).to[MIndexedSeq]
    val mapCarts = Buffer.empty[Cart]
    for ( y <- mapData.indices; x <- mapData(0).indices ) {
      mapData(y)(x) match {
        case '^' =>
          mapCarts += Cart(y, x, Up, 0)
          mapData(y)(x) = '|'
        case 'v' =>
          mapCarts += Cart(y, x, Down, 0)
          mapData(y)(x) = '|'
        case '<' =>
          mapCarts += Cart(y, x, Left, 0)
          mapData(y)(x) = '-'
        case '>' =>
          mapCarts += Cart(y, x, Right, 0)
          mapData(y)(x) = '-'
        case _ => ()
      }
    }
    println()
    Map(
      mapData.map(_.toIndexedSeq).toIndexedSeq,
      mapCarts.sorted.toIndexedSeq)
  }

  def problem1() = try {
    val m = map
    while ( true ) m.tick()
  } catch {
    case Crash(x, y) => print(s"$x,$y")
  }

  def problem2() = {
    val m = map
    while ( m.carts.size > 1 ) {
      m.tick(remove = true)
    }
    val c = m.carts(0)
    print(s"${c.x},${c.y}")
  }

}
