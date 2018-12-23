package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day23 extends AoCBase(23) {

  case class Point(x: Long, y: Long, z: Long) {
    def dist(that: Point): Long =
      math.abs(x - that.x) + math.abs(y - that.y) + math.abs(z - that.z)
  }

  case class Bot(p: Point, r: Long)

  val botPat = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

  def data: Seq[Bot] = rawData.map({
    case botPat(x, y, z, r) =>
      Bot(Point(x.toLong, y.toLong, z.toLong), r.toLong)
  }).toSeq

  def problem1() = {
    val b1 = data.maxBy(_.r)
    print(data.count(x => (x.p dist b1.p) <= b1.r))
  }

  def problem2() = {
    def dataDerez(n: Int) = if ( n == 0 ) data else {
      val f = math.pow(10.0, n.toDouble).toLong
      data.map({
        case Bot(Point(x, y, z), r) => Bot(Point(x/f, y/f, z/f), r/f + 1)
      })
    }
    def minMaxes(ps: Seq[Point]): (Long, Long, Long, Long, Long, Long) =
      ( ps.map(_.x).min, ps.map(_.x).max,
        ps.map(_.y).min, ps.map(_.y).max,
        ps.map(_.z).min, ps.map(_.z).max )
    var (xMin, xMax, yMin, yMax, zMin, zMax) = minMaxes(dataDerez(7).map(_.p))
    var dr = 7
    while ( dr >= 0 ) {
      val d = dataDerez(dr)
      val c =
        for { z <- zMin to zMax; y <- yMin to yMax; x <- xMin to xMax }
        yield(d.count(b => (b.p dist Point(x, y, z)) <= b.r), Point(x, y, z))
      val md = c.map((d, _) => d).max
      val mps = c.filter((d, _) => d == md).map((_, p) => p)
      if ( dr > 0 ) {
        val (nxMin, nxMax, nyMin, nyMax, nzMin, nzMax) = minMaxes(mps)
        xMin = (nxMin - 1) * 10
        xMax = (nxMax + 1) * 10
        yMin = (nyMin - 1) * 10
        yMax = (nyMax + 1) * 10
        zMin = (nzMin - 1) * 10
        zMax = (nzMax + 1) * 10
      } else {
        // Should be down to the real result now
        print(Point(0, 0, 0) dist mps.head)
      }
      dr = dr - 1
    }
  }

}
