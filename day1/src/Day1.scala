import scala.io.Source

object Day1 {

  def data: Iterator[Int] =
    Source.fromFile("day1/input.txt").getLines.map(_.toInt)

  def problem1(): Unit =
    println(s"  Problem 1 solution: ${data.sum}")

  def problem2(): Unit = {
    def deltas = Iterator.continually(data).flatten
    def sums = deltas.scanLeft(0)(_ + _)
    def seen = sums.scanLeft(Set.empty)(_ + _)
    def dups = (seen zip sums).filter(_ contains _).map((_, sum) => sum)
    println(s"  Problem 2 solution: ${dups.next}")
  }

  def main(args: Array[String]): Unit = {
    println("Day 1:")
    problem1()
    problem2()
  }

}
