import scala.io.Source

object Day2 {

  def data: Stream[String] =
    Source.fromFile("day2/input.txt").getLines.toStream

  def problem1(): Unit = {
    implicit class StringHasNDups(self: String) {
      def hasExactlyNDuplicates(n: Int): Boolean =
        self.groupBy(identity).exists((_, v) => v.size == n)
    }
    val has2Count = data.count(_.hasExactlyNDuplicates(2))
    val has3Count = data.count(_.hasExactlyNDuplicates(3))
    println(s"  Problem 1 solution: ${has2Count * has3Count}")
  }

  def problem2(): Unit = {
    implicit class StringDropIndex(self: String) {
      def dropIndex(n: Int): String = self.take(n) ++ self.drop(n + 1)
    }
    val idsDropOne = for {
      id <- data
      idDropOne <- (0 to id.size).map(i => id.dropIndex(i)).distinct
    } yield idDropOne
    val matches =
      idsDropOne.groupBy(identity).filter((_, v) => v.size == 2).keySet

    assert(matches.size == 1)
    println(s"  Problem 2 solution: ${matches.head}")
  }

  def main(args: Array[String]): Unit = {
    println("Day 2:")
    problem1()
    problem2()
  }

}
