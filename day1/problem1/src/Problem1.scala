import scala.io.Source

object Problem1 {
  def main(args: Array[String]): Unit = {
    if ( args.length != 1 ) {
      println("Usage: mill day1.problem1.run FILENAME")
      System.exit(-1)
    }
    val filename = args(0)

    println(Source.fromFile(filename).getLines.map(_.toInt).sum)

  }
}
