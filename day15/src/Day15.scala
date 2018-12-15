package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.IndexedSeq
import scala.collection.mutable.{IndexedSeq => MIndexedSeq, Queue}

object Day15 extends AoCBase(15) {

  case class Done(s: State) extends Exception

  // Grid //////////////////////////////////////////////////////////////

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

  // State /////////////////////////////////////////////////////////////

  type State = Seq[Mob]
  val Z = Int.MaxValue

  private[this] implicit class StateOps(self: State) {
    def apply(p: Position): Option[Mob] = self.find(_.p == p)
    def blocked(p: Position): Boolean = terrain(p) || self(p).isDefined
    def show(): Unit =
      for ( (row, y) <- terrain.zipWithPosition.zipWithIndex ) {
        println(s"${
          row.map({
            case (true, _) => '#'
            case (false, p) =>
              self(p).map(_.k.toChar).getOrElse('.')
          }).mkString("")
        }   ${
          self.filter(_.p.y == y).sorted.map(_.stats).mkString(", ")
        }")
      }
    def distanceGrid(p: Position): Grid[Int] = {
      val dGrid = terrain.mapCells(_ => Z).mutable
      val q = Queue((p, 0))
      dGrid(p) = 0
      while ( q.nonEmpty ) {
        val (p, d) = q.dequeue()
        for ( a <- p.adjacent; if dGrid(a) == Z; if !blocked(a) ) {
          dGrid(a) = d + 1
          q.enqueue((a, d + 1))
        }
      }
      dGrid
    }
    def move(m: Mob): Unit = {
      import scala.language.implicitConversions
      val e = self.filterNot(_.k == m.k)
      if ( e.isEmpty ) throw Done(self)
      if ( m.p.adjacent.flatMap(self(_)).exists(_.k != m.k) ) return ()
      val mDist = self.distanceGrid(m.p)
      e.flatMap(_.p.adjacent).distinct.map(a => (mDist(a), a)).
        filter((d, _) => d != Z).sorted.headOption.map((_, a) => a) match {
          case None => ()
          case Some(target) =>
            val tDist = self.distanceGrid(target)
            val (_, t) = m.p.adjacent.toSeq.map(a => (tDist(a), a)).sorted.head
            m.p = t
        }
    }
    def attack(m: Mob): Unit = {
      import scala.language.implicitConversions
      m.p.adjacent.flatMap(self(_)).filter(_.k != m.k).toSeq.
        sorted(Mob.hpOrdering).headOption match {
          case None => ()
          case Some(t) => t.hp -= m.ap
        }
    }
    def tick(): State = {
      var s = self
      for ( m <- self.sorted ) {
        if ( m.hp > 0 ) {
          s.move(m)
          s.attack(m)
          s = s.filter(_.hp > 0)
        }
      }
      s
    }
  }

  //////////////////////////////////////////////////////////////////////

  val data: Grid[Char] = rawData.toIndexedSeq.map(_.toIndexedSeq)
  val terrain: Grid[Boolean] = data.mapCells(_ == '#')
  def init: State = for {
    (k, p) <- data.zipWithPosition.flatten.toSeq
    if k == 'E' || k == 'G'
  } yield Mob(p, Kind(k))

  def simulate(elfAp: Int = 3): (State, Int, Boolean) = {
    var s = init.map(m => if ( m.k == Kind.Goblin ) m else m.copy(ap = elfAp))
    val initElves = s.count(_.k == Kind.Elf)
    var i = 0
    try {
      while ( true ) {
        s = s.tick()
        i += 1
      }
    } catch {
      case Done(s) => ()
    }
    val finElves = s.count(_.k == Kind.Elf)
    (s, i, finElves < initElves)
  }

  def problem1() = {
    val (s, i, _) = simulate()
    println(s"Round $i")
    s.show()
    println(i * s.map(_.hp).sum)
  }

  def problem2() = {
    var elfAp = 3
    while ( true ) {
      val (s, i, elvesDied) = simulate(elfAp)
      if ( !elvesDied ) {
        println(s"Elf attack power $elfAp")
        println(s"Round $i")
        s.show()
        println(i * s.map(_.hp).sum)
        return ()
      }
      elfAp += 1
    }
  }

}
