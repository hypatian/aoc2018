import scala.io.Source

object Problem1 {
  def main(args: Array[String]): Unit = {
    if ( args.length != 1 ) {
      println("Usage: mill day2.problem1.run FILENAME")
      System.exit(-1)
    }
    val filename = args(0)

    implicit class StringHasNDups(self: String) {
      def hasExactlyNDuplicates(n: Int): Boolean =
        self.groupBy(identity).exists((_, v) => v.size == n)
    }

    val data = Source.fromFile(filename).getLines.toStream
    val has2Count = data.count(_.hasExactlyNDuplicates(2))
    val has3Count = data.count(_.hasExactlyNDuplicates(3))

    println(has2Count * has3Count)

  }
}
