package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc._
import scala.collection.mutable.Map

object Day20 extends AoCBase(20) {

  case class Room(
    var n: Boolean, var s: Boolean, var w: Boolean, var e: Boolean)
  object Room {
    def apply(): Room = Room(false, false, false, false)
  }

  val rooms = Map.empty[(Int, Int), Room]

  def show(): Unit = {
    val minX = rooms.keys.map((x, _) => x).min
    val maxX = rooms.keys.map((x, _) => x).max
    val minY = rooms.keys.map((_, y) => y).min
    val maxY = rooms.keys.map((_, y) => y).max
    for ( y <- minY to maxY ) {
      for ( x <- minX to maxX ) rooms.lift((x, y)) match {
        case Some(Room(true, _, _, _)) => print("#-")
        case _                         => print("##")
      }
      println("#")
      for ( x <- minX to maxX ) rooms.lift((x, y)) match {
        case Some(Room(_, _, true, _)) if ( x == 0 && y == 0 ) => print("|X")
        case Some(Room(_, _, true, _))                         => print("|.")
        case Some(_)                   if ( x == 0 && y == 0 ) => print("#X")
        case Some(_)                                           => print("#.")
        case None                      => print("##")
      }
      println("#")
    }
    for ( x <- minX to maxX ) print("##")
    println("#")
  }

  type R = (Set[(Int, Int)], Seq[Char])

  def move(dir: Char, xy: (Int, Int)): (Int, Int) = {
    val (x, y) = xy
    dir match {
      case 'N' =>
        rooms.getOrElseUpdate((x, y), Room()).n = true
        rooms.getOrElseUpdate((x, y - 1), Room()).s = true
        (x, y - 1)
      case 'S' =>
        rooms.getOrElseUpdate((x, y), Room()).s = true
        rooms.getOrElseUpdate((x, y + 1), Room()).n = true
        (x, y + 1)
      case 'W' =>
        rooms.getOrElseUpdate((x, y), Room()).w = true
        rooms.getOrElseUpdate((x - 1, y), Room()).e = true
        (x - 1, y)
      case 'E' =>
        rooms.getOrElseUpdate((x, y), Room()).e = true
        rooms.getOrElseUpdate((x + 1, y), Room()).w = true
        (x + 1, y)
    }
  }

  def parseDoor(s: Seq[Char], xys: Set[(Int, Int)]): R =
    (xys.map(xy => move(s.head, xy)), s.tail)

  def parseSeq(s: Seq[Char], xys: Set[(Int, Int)]): R = s.head match {
    case '|' | ')' | '$' => (xys, s)
    case '(' =>
      val (newXys, rest) = parseAlt(s.tail, xys, Set.empty)
      parseSeq(rest, newXys)
    case _ =>
      val (newXys, rest) = parseDoor(s, xys)
      parseSeq(rest, newXys)
  }

  def parseAlt(s: Seq[Char], xys: Set[(Int, Int)], cont: Set[(Int, Int)]): R =
    s.head match {
      case '|' => parseAlt(s.tail, xys, cont)
      case ')' | '$' =>
        (cont, s.tail)
      case _ =>
        val (newXys, rest) = parseSeq(s, xys)
        parseAlt(rest, xys, cont ++ newXys)
    }

  def parse(s: Seq[Char]): Unit = s.head match {
    case '^' => parseAlt(s.tail, Set((0, 0)), Set.empty) match {
      case (_, Seq()) => ()
    }
  }

  def distanceMap(): Map[(Int, Int), Int] = {
    import scala.collection.mutable.{Map, Queue}
    val distMap = Map((0, 0) -> 0)
    val q = Queue((0, 0) -> 0)
    def adjacent(x: Int, y: Int): Seq[(Int, Int)] =
      Seq(
        (if ( rooms((x, y)).n ) Some(x, y-1) else None),
        (if ( rooms((x, y)).s ) Some(x, y+1) else None),
        (if ( rooms((x, y)).w ) Some(x-1, y) else None),
        (if ( rooms((x, y)).e ) Some(x+1, y) else None)).flatten
    while ( q.nonEmpty ) {
      val ((x, y), d) = q.dequeue()
      for ( (x2, y2) <- adjacent(x, y) ) {
        if ( ! distMap.contains((x2, y2)) ) {
          distMap((x2, y2)) = d + 1
          q.enqueue((x2, y2) -> (d + 1))
        } else {
          assert(distMap((x2, y2)) <= (d + 1))
        }
      }
    }
    distMap
  }

  def problem1() = {
    parse(rawData.next.toSeq)
    val dist = distanceMap()
    print(dist.values.max)
  }

  def problem2() = {
    val dist = distanceMap()
    print(dist.values.count(_ >= 1000))
  }

}
