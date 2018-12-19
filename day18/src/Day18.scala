package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.{IndexedSeq => MIndexedSeq}

object Day18 extends AoCBase(18) {

  type Grid[T] = IndexedSeq[IndexedSeq[T]]
  private[this] implicit class GridOps[T](self: Grid[T]) {
    def height: Int = self.size
    def width: Int = self(0).size
  }

  val OPEN: Byte = 0
  val TREES: Byte = 1
  val LUMBERYARD: Byte = 2

  val init: Grid[Byte] = rawData.map(_.map({
    case '.' => OPEN
    case '|' => TREES
    case '#' => LUMBERYARD
  }).toIndexedSeq).toIndexedSeq

  def newState(g: Grid[Byte])(y: Int, x : Int): Byte = {
    val w = g.width
    val h = g.height
    val a = for {
      dy <- -1 to 1
      dx <- -1 to 1
      if dy != 0 || dx != 0
      if x + dx >= 0 && x + dx < w
      if y + dy >= 0 && y + dy < h
    } yield g(y + dy)(x + dx)
    g(y)(x) match {
      case OPEN => if ( a.count(_ == TREES) >= 3 ) TREES else OPEN
      case TREES => if ( a.count(_ == LUMBERYARD) >= 3 ) LUMBERYARD else TREES
      case LUMBERYARD =>
        if ( a.count(_ == LUMBERYARD) >= 1 && a.count(_ == TREES) >= 1 )
          LUMBERYARD else OPEN
    }
  }

  def show(g: Grid[Byte]): Unit =
    for ( row <- g ) {
      println(row.map({
        case OPEN => '.'
        case TREES => '|'
        case LUMBERYARD => '#'
      }).mkString(""))
    }

  def problem1() = {
    var s = init
    for ( i <- 1 to 10 )
      s = IndexedSeq.tabulate(s.height, s.width)(newState(s))
    val wooded = s.flatten.count(_ == TREES)
    val lumberyards = s.flatten.count(_ == LUMBERYARD)
    val value = wooded * lumberyards
    print(value)
  }

  def problem2() = {
    import scala.collection.mutable.Map
    val stateHashes = Map.empty[Int, (Long, Grid[Byte])]
    var s = init
    stateHashes(s.hashCode) = (0L, s)
    // Calculated by the below hashing:
    // First repeat: 448 = 476
    // (1000000000L - 448) % 28 + 448 = 468
//    for ( i <- 1L to 1000000000L) {
    for ( i <- 1L to 468L) {
      s = IndexedSeq.tabulate(s.height, s.width)(newState(s))
      val h = s.hashCode
      if ( stateHashes.contains(h) ) {
        val (oi, os) = stateHashes(h)
        println(s"REPEAT FOUND, #$i and #$oi")
        println(s"#$oi ==>")
        show(os)
        println(s"#$i ==>")
        show(s)
        return ()
      }
      stateHashes(h) = (i, s)
    }
    val wooded = s.flatten.count(_ == TREES)
    val lumberyards = s.flatten.count(_ == LUMBERYARD)
    val value = wooded * lumberyards
    print(value)
  }

}
