package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day25 extends AoCBase(25) {

  val pointPat = """(-?\d+),(-?\d+),(-?\d+),(-?\d+)""".r

  case class Point(x: Int, y: Int, z: Int, t: Int) {
    def dist(o: Point): Int =
      ( math.abs(x - o.x) + math.abs(y - o.y) +
        math.abs(z - o.z) + math.abs(t - o.t) )
  }

  val data: Seq[Point] = rawData.map({
    case pointPat(x, y, z, w) => Point(x.toInt, y.toInt, z.toInt, w.toInt)
  }).toSeq

  def problem1() = {
    import scala.collection.mutable.Map
    val equiv: Map[Int, Int] = Map(data.indices.map(i => (i, i)).toSeq: _*)
    val eqset: Map[Int, Seq[Int]] =
      Map(data.indices.map(i => (i, Seq(i))).toSeq: _*)
    // I can do this smarter. Maybe later.
    def unify(a: Int, b: Int): Unit = {
      val ea = equiv(a min b)
      val eb = equiv(a max b)
      if ( ea != eb ) {
        eqset(ea) = eqset(ea) ++ eqset(eb)
        for ( k <- eqset(eb) ) equiv(k) = ea
        eqset(eb) = Seq.empty
      }
    }
    for {
      (p1, i1) <- data.zipWithIndex
      (p2, i2) <- data.take(i1).zipWithIndex
      if (p1 dist p2) <= 3
    } {
      unify(i2, i1)
    }
    print(equiv.values.toSeq.distinct.size)
  }

  def problem2() = ()

}
