package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.{IndexedSeq => MIndexedSeq}

object Day17 extends AoCBase(17) {

  case class Position(x: Int, y: Int)

  type Grid[T] = IndexedSeq[IndexedSeq[T]]
  type MGrid[T] = MIndexedSeq[MIndexedSeq[T]]
  private[this] implicit class GridOps[T](self: Grid[T]) {
    def height: Int = self.size
    def width: Int = self(0).size
    def positions: Iterator[Position] = for {
      y <- self.indices.iterator
      x <- self(0).indices
    } yield Position(x, y)
    def apply(p: Position): T = self(p.y)(p.x)
    def mapCells[U](f: T => U): Grid[U] = self.map(_.map(f))
    val zipWithPosition: Iterator[Iterator[(T, Position)]] = for {
      (r, y) <- self.iterator.zipWithIndex
    } yield for {
      (v, x) <- r.iterator.zipWithIndex
    } yield (v, Position(x, y))
    def mutable: MGrid[T] = self.map(_.to[MIndexedSeq]).to[MIndexedSeq]
  }
  private[this] implicit class MGridOps[T](self: MGrid[T]) {
    def update(p: Position, v: T): Unit = self(p.y)(p.x) = v
  }

////////////////////////////////////////////////////////////////////////

  val SAND: Byte = 0
  val DAMP: Byte = 1
  val WATER: Byte = 2
  val CLAY: Byte = 3

  def show(grid: Grid[Byte]): Unit = for ( (row, y) <- grid.zipWithIndex ) {
    println(row.map({
      case SAND => '.'
      case DAMP => '|'
      case CLAY => '#'
      case WATER => '~'
    }).mkString("") + (if ( y < minY || y > maxY ) " [NC]" else ""))
  }

////////////////////////////////////////////////////////////////////////

  val yRangePat = """x=(\d+), y=(\d+)\.\.(\d+)""".r
  val xRangePat = """y=(\d+), x=(\d+)\.\.(\d+)""".r

  def parseLine(s: String): (Range, Range) = s match {
    case yRangePat(x, y1, y2) => (x.toInt to x.toInt, y1.toInt to y2.toInt)
    case xRangePat(y, x1, x2) => (x1.toInt to x2.toInt, y.toInt to y.toInt)
  }

  val data: Seq[(Range, Range)] = rawData.map(parseLine).toSeq
  val minX = data.map(_._1.head).min - 1
  val maxX = data.map(_._1.last).max + 1
  val width = maxX - minX
  val minY = data.map(_._2.head).min
  val maxY = data.map(_._2.last).max
  val normData: Seq[(Range, Range)] =
    data.map((xr, yr) => ((xr.head - minX) to (xr.last - minX), yr))

  def fillAcross(grid: MGrid[Byte], x: Int, y: Int): Unit = {
    var leftSide: Option[Int] = None
    var rightSide: Option[Int] = None
    var offLeft = false
    var offRight = false
    var sx = x
    while ( leftSide.isEmpty && !offLeft ) {
      grid(y)(sx) = DAMP
      if ( grid(y + 1)(sx) == SAND ) fillDown(grid, sx, y + 1)
      if ( grid(y + 1)(sx) <= DAMP ) {
        offLeft = true
      } else if ( grid(y)(sx - 1) > DAMP ) {
        leftSide = Some(sx)
      } else {
        sx -= 1
      }
    }
    sx = x
    while ( rightSide.isEmpty && !offRight ) {
      grid(y)(sx) = DAMP
      if ( grid(y + 1)(sx) == SAND ) fillDown(grid, sx, y + 1)
      if ( grid(y + 1)(sx) <= DAMP ) {
        offRight = true
      } else if ( grid(y)(sx + 1) > DAMP ) {
        rightSide = Some(sx)
      } else {
        sx += 1
      }
    }
    if ( leftSide.isDefined && rightSide.isDefined ) {
      for ( sx <- leftSide.get to rightSide.get ) {
        grid(y)(sx) = WATER
      }
    }
  }

  def fillDown(grid: MGrid[Byte], x: Int, y: Int): Unit = {
    if ( y <= maxY && grid(y)(x) == CLAY ) return ()
    if ( y <= maxY && grid(y)(x) <= DAMP ) {
      grid(y)(x) = DAMP
      fillDown(grid, x, y + 1)
    }
    if ( y < maxY && grid(y + 1)(x) > DAMP ) {
      fillAcross(grid, x, y)
    }
  }

  var wetCount = 0
  var waterCount = 0

  def problem1() = {
    import scala.collection.mutable.IndexedSeq
    val grid = IndexedSeq.fill[Byte](maxY + 1, width + 1)(SAND)
    for ( (xr, yr) <- normData; y <- yr; x <- xr ) grid(y)(x) = CLAY
    fillDown(grid, 500 - minX, 0)
    for ( y <- minY to maxY; c <- grid(y) ) c match {
      case DAMP =>
        wetCount += 1
      case WATER =>
        wetCount += 1
        waterCount += 1
      case _ => ()
    }
    print(wetCount)
  }

  def problem2() = print(waterCount)

}
