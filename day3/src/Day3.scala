import scala.io.Source

object Day3 {

  case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int)
  object Claim {
    val dataPattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
    def apply(s: String): Claim = s match {
      case dataPattern(id, x, y, w, h) =>
        Claim(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
    }
  }

  class Fabric {
    val data = Array.ofDim[Short](1000 * 1000)
    def incr(c: Claim): Unit =
      for ( y <- c.y until (c.y + c.h) )
        for ( x <- c.x until (c.x + c.w) )
          data(x + y * 1000) = (data(x + y * 1000) + 1).toShort
    def check(c: Claim): Boolean = {
      for ( y <- c.y until (c.y + c.h) )
        for ( x <- c.x until (c.x + c.w) )
          if ( data(x + y * 1000) > 1 ) return false
      true
    }
  }

  def data: Iterator[Claim] =
    Source.fromFile("day3/input.txt").getLines.map(Claim(_))

  def problem1(): Unit = {
    val fabric = new Fabric
    for ( claim <- data ) fabric.incr(claim)
    println(s"  Problem 1 solution: ${fabric.data.count(_ > 1)}")
  }

  def problem2(): Unit = {
    val fabric = new Fabric
    for ( claim <- data ) fabric.incr(claim)
    for ( claim <- data )
      if ( fabric.check(claim) )
        println(s"  Problem 2 solution: ${claim.id}")
  }

  def main(args: Array[String]): Unit = {
    println("Day 3:")
    problem1()
    problem2()
  }

}
