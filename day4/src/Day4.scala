import scala.io.Source
import scala.collection.mutable.{IndexedSeq, Map}

object Day4 {

  sealed trait Record
  case class BeginsShift(guardId: Int) extends Record
  case class FallsAsleep(day: String, minute: Int) extends Record
  case class WakesUp(day: String, minute: Int) extends Record

  sealed trait Status
  case object Awake extends Status
  case object Asleep extends Status

  object Record {
    val beginsPat = """\[1518-\d+-\d+ \d+:\d+\] Guard #(\d+) begins shift""".r
    val fallsAsleepPat = """\[1518-(\d+-\d+) 00:(\d+)\] falls asleep""".r
    val wakesUpPat = """\[1518-(\d+-\d+) 00:(\d+)\] wakes up""".r
    def apply(s: String): Record = s match {
      case beginsPat(guardId) => BeginsShift(guardId.toInt)
      case fallsAsleepPat(day, minute) => FallsAsleep(day, minute.toInt)
      case wakesUpPat(day, minute) => WakesUp(day, minute.toInt)
    }
  }

  def data =
    Source.fromFile("day4/input.txt").getLines.toSeq.sorted.map(Record(_))

  def statusMap: Map[Int, Map[String, IndexedSeq[Status]]] = {
    var currentGuardId = -1
    var currentDay = "NONE"
    var currentMinute = -1
    var currentStatus: Status = Awake
    var guardStatus: Map[Int, Map[String, IndexedSeq[Status]]] = Map.empty
    def fillSleep(start: Int, end: Int): Unit =
      for ( i <- start until end )
        guardStatus(currentGuardId)(currentDay)(i) = Asleep
    def finishShift(): Unit =
      if ( currentStatus == Asleep ) fillSleep(currentMinute, 60)
    def startShift(guardId: Int): Unit = {
      currentGuardId = guardId
      currentDay = "NONE"
      currentMinute = 0
      currentStatus = Awake
      if ( ! guardStatus.contains(guardId) ) guardStatus(guardId) = Map.empty
    }
    def fallsAsleep(day: String, minute: Int): Unit = {
      if ( ! guardStatus(currentGuardId).contains(day) )
        guardStatus(currentGuardId)(day) = IndexedSeq.fill(60)(Awake)
      currentDay = day
      currentMinute = minute
      currentStatus = Asleep
    }
    def wakesUp(day: String, minute: Int): Unit = {
      assert(day == currentDay)
      fillSleep(currentMinute, minute)
      currentMinute = minute
      currentStatus = Awake
    }
    for ( record <- data ) record match {
      case BeginsShift(guardId) =>
        finishShift()
        startShift(guardId)
      case FallsAsleep(day, minute) => fallsAsleep(day, minute)
      case WakesUp(day, minute) => wakesUp(day, minute)
    }
    guardStatus
  }

  def problem1(): Unit = {
    val guardStatus = statusMap
    // Find the most asleep guard
    val guardSleepiness = for {
      (guardId, days) <- guardStatus
    } yield (guardId, days.map((_, minutes) => minutes.count(_ == Asleep)).sum)
    val sleepiestGuard =
      guardSleepiness.toSeq.sorted(Ordering.by((_, s) => s)).last._1

    // Find their sleepiest minute
    val minuteSleepiness: Map[Int, Int] = Map.empty
    for {
      (day, minuteStatus) <- guardStatus(sleepiestGuard).toSeq
      (Asleep, minute) <- minuteStatus.zipWithIndex
    } {
      minuteSleepiness(minute) = minuteSleepiness.getOrElse(minute, 0) + 1
    }
    val sleepiestMinute =
      minuteSleepiness.toSeq.sorted(Ordering.by((m, v) => (v, m))).last._1
    println(s"  Problem 1 solution: ${sleepiestGuard * sleepiestMinute}")

  }

  def problem2(): Unit = {
    val guardStatus = statusMap
    val guardSleepMinutes = for {
      (guardId, days) <- guardStatus.toSeq
      (day, minuteStatus) <- days.toSeq
      (Asleep, minute) <- minuteStatus.zipWithIndex
    } yield (guardId, minute)
    val biggestGuardMinute =
      guardSleepMinutes.groupBy(identity).mapValues(_.count(_ => true)).
        toSeq.sorted(Ordering.by((_, repeats) => repeats)).last
    val ((id, min), _) = biggestGuardMinute
    println(s"  Problem 2 solution: ${min * id}")
  }

  def main(args: Array[String]): Unit = {
    println("Day 4:")
    problem1()
    problem2()
  }

}
