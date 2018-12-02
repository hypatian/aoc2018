import scala.io.Source

object Problem2 {
  def main(args: Array[String]): Unit = {
    if ( args.length != 1 ) {
      println("Usage: mill day2.problem2.run FILENAME")
      System.exit(-1)
    }
    val filename = args(0)

    implicit class StringDropIndex(self: String) {
      def dropIndex(n: Int): String = self.take(n) ++ self.drop(n + 1)
    }

    val idsDropOne = for {
      id <- Source.fromFile(filename).getLines.toStream
      idDropOne <- (0 to id.size).map(i => id.dropIndex(i)).distinct
    } yield idDropOne

    val matches =
      idsDropOne.groupBy(identity).filter((_, v) => v.size == 2).keySet

    assert(matches.size == 1)
    println(matches.head)

  }
}
