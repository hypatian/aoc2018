import scala.io.Source

object Problem2 {
  def main(args: Array[String]): Unit = {
    if ( args.length != 1 ) {
      println("Usage: mill day1.problem2.run FILENAME")
      System.exit(-1)
    }

    val filename = args(0)
    def deltas = Iterator.continually {
      Source.fromFile(filename).getLines.map(_.toInt)
    }.flatten
    def sums = deltas.scanLeft(0)(_ + _)
    def seen = sums.scanLeft(Set.empty)(_ + _)
    def dups = (seen zip sums).filter(_ contains _).map(_._2)
    println(dups.next())
  }

}
