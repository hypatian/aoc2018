package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.Map

object Day22 extends AoCBase(22) {

  val DEPTH = 7305
  val TARGET = (13, 734)
  //val DEPTH = 510
  //val TARGET = (10, 10)
  //val DEPTH = 11394
  //val TARGET = (7, 701)

  enum RegionType { case Rocky, Wet, Narrow }
  import RegionType.{Rocky, Wet, Narrow}

  enum Tool { case Torch, Climbing, Neither }
  import Tool.{Torch, Climbing, Neither}

  def allowedTool(r: RegionType, t: Tool): Boolean = (r, t) match {
    case (Rocky, Neither) => false
    case (Wet, Torch) => false
    case (Narrow, Climbing) => false
    case _ => true
  }

  def show(): Unit = {
    println()
    for ( y <- 0 to TARGET._2 ) {
      for (x <- 0 to TARGET._1 ) {
        regionType(x, y) match {
          case Rocky => print(".")
          case Wet => print("=")
          case Narrow => print("|")
        }
      }
      println()
    }
  }

  def mmod(a: Int, b: Int): Int = (a % b + b) % b

  private[this] val geoIndexCache: Map[(Int, Int), Int] = Map.empty
  def geoIndex(x: Int, y: Int): Int = {
    geoIndexCache.getOrElseUpdate((x, y), (x, y) match {
      case (0, 0) => 0
      case TARGET => 0
      case (x, 0) => x * 16807
      case (0, y) => y * 48271
      case (x, y) => erosionLevel(x-1, y) * erosionLevel(x, y-1)
    })
  }
  def erosionLevel(x: Int, y: Int): Int = mmod(geoIndex(x, y) + DEPTH, 20183)
  def regionType(x: Int, y: Int): RegionType =
    RegionType.enumValue(mmod(erosionLevel(x, y), 3))
  def riskLevel(x1: Int, y1: Int, x2: Int, y2: Int): Int =
    (for ( x <- x1 to x2; y <- y1 to y2 ) yield regionType(x, y).enumTag).sum

  def problem1() = { show();
    print(riskLevel(0, 0, TARGET._1, TARGET._2))
  }

  case class State(x: Int, y: Int, tool: Tool)

  def adjacent(x: Int, y: Int): Seq[(Int, Int)] =
    Seq((x+1, y), (x, y+1), (x-1, y), (x, y-1))

  def toolChanges(t: Int, s: State): Seq[(Int, State)] =
    for {
      newTool <- Tool.enumValues.toSeq
      if newTool != s.tool
      if allowedTool(regionType(s.x, s.y), newTool)
    } yield (t + 7, s.copy(tool = newTool))

  def moves(t: Int, s: State): Seq[(Int, State)] =
    for {
      (nx, ny) <- adjacent(s.x, s.y)
      if nx >= 0 && ny >= 0
      if allowedTool(regionType(nx, ny), s.tool)
    } yield (t + 1, s.copy(x = nx, y = ny))
  def actions(t: Int, s: State): Seq[(Int, State)] =
    toolChanges(t, s) ++ moves(t, s)

  def search(): Int = {
    import scala.collection.mutable.PriorityQueue
    val init = State(0, 0, Torch)
    val target = State(TARGET._1, TARGET._2, Torch)
    def tsToT(ts: (Int, State)): Int = ts._1
    val q = PriorityQueue.empty(Ordering.by(tsToT).reverse)
    q.enqueue(0 -> init)
    val d = Map(init -> 0)
    while ( q.nonEmpty ) {
      val (t, s) = q.dequeue()
      if ( s == target ) return t
      for {
        (nt, ns) <- actions(t, s)
        if d.getOrElse(ns, Int.MaxValue) > nt
      } {
        d(ns) = nt
        q.enqueue(nt -> ns)
      }
    }
    return -1
  }

  def problem2() = print(search())

}
