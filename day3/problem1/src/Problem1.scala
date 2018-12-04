import scala.io.Source

object Problem1 {
  def main(args: Array[String]): Unit = {
    if ( args.length != 1 ) {
      println("Usage: mill day3.problem1.run FILENAME")
      System.exit(-1)
    }
    val filename = args(0)

    case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int)
    object Claim {
      val dataPattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
      def apply(s: String): Claim = s match {
        case dataPattern(id, x, y, w, h) =>
          Claim(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
      }
    }

    val fabric = Array.ofDim[Short](1000 * 1000)
    def incrFabric(c: Claim): Unit =
      for ( y <- c.y until (c.y + c.h) )
        for ( x <- c.x until (c.x + c.w) )
          fabric(x + y * 1000) = (fabric(x + y * 1000) + 1).toShort

    for ( claim <- Source.fromFile(filename).getLines.map(Claim(_)) ) {
      incrFabric(claim)
    }

    println(fabric.count(_ > 1))

  }
}
