import scala.io.Source
import scala.collection.mutable.{IndexedSeq, Map}

object Day4 {

  val BeginsShift = """\[[^\]]+\] Guard #(\d+) begins shift""".r
  val FallsAsleep = """\[[^:]+:(\d\d)\] falls asleep""".r
  val WakesUp     = """\[[^:]+:(\d\d)\] wakes up""".r

  def data = Source.fromFile("day4/input.txt").getLines

  type GuardId = Int
  type Shift = Seq[Boolean]

  lazy val guardShifts: Map[GuardId, Seq[Shift]] = {
    val guardShifts: Map[GuardId, Seq[Shift]] = Map.empty
    var fellAsleep = -1
    var shift = IndexedSeq.empty[Boolean]
    for ( s <- data.toSeq.sorted ) s match {
      case BeginsShift(id) =>
        fellAsleep = -1
        shift = IndexedSeq.fill(60)(false)
        guardShifts(id.toInt) =
          shift +: guardShifts.getOrElse(id.toInt, List.empty)
      case FallsAsleep(minute) =>
        fellAsleep = minute.toInt
      case WakesUp(minute) =>
        for ( i <- fellAsleep until minute.toInt ) shift(i) = true
    }
    guardShifts
  }

  def problem1(): Unit = {
    val guardSleepiness = for {
      (guardId, shifts) <- guardShifts.toSeq
    } yield (shifts.map(_.count(identity)).sum, guardId)
    val sleepiestGuardId = guardSleepiness.sorted.last._2
    val minuteSleepiness = (for {
      shift <- guardShifts(sleepiestGuardId)
      (true, minute) <- shift.zipWithIndex
    } yield minute).groupBy(identity).mapValues(_.count(_ => true)).
      toSeq.map((minute, count) => (count, minute))
    val sleepiestMinute = minuteSleepiness.sorted.last._2
    println(s"  Problem 1 solution: ${sleepiestGuardId * sleepiestMinute}")
  }

  def problem2(): Unit = {
    val guardSleepMinutes = for {
      (guardId, shifts) <- guardShifts.toSeq
      shift <- shifts
      (true, minute) <- shift.zipWithIndex
    } yield (guardId, minute)
    val biggestGuardMinute =
      guardSleepMinutes.groupBy(identity).mapValues(_.count(_ => true)).
        toSeq.sorted(Ordering.by((_, repeats) => repeats)).last._1
    val (guardId, minute) = biggestGuardMinute
    println(s"  Problem 2 solution: ${guardId * minute}")
  }

  def main(args: Array[String]): Unit = {
    println("Day 4:")
    problem1()
    problem2()
  }

}
