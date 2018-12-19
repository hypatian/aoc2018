import mill._, scalalib._

trait MyScalaModule extends ScalaModule {
  def scalaVersion = "0.11.0-RC1"
  def scalacOptions = Seq("-deprecation", "-feature")
  def forkArgs = Seq("-Xmx16g")
}

object aocbase extends MyScalaModule {
  def run(args: String*) = T.command { }
}

trait AocModule extends MyScalaModule {
  def moduleDeps = Seq(aocbase)
}

object day1 extends AocModule
object day2 extends AocModule
object day3 extends AocModule
object day4 extends AocModule
object day5 extends AocModule
object day6 extends AocModule
object day7 extends AocModule
object day8 extends AocModule
object day9 extends AocModule
object day10 extends AocModule
object day11 extends AocModule
object day12 extends AocModule
object day13 extends AocModule
object day14 extends AocModule
object day15 extends AocModule
object day16 extends AocModule
object day17 extends AocModule
object day18 extends AocModule
object day19 extends AocModule
