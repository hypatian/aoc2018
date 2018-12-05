import mill._, scalalib._

trait MyScalaModule extends ScalaModule {
  def scalaVersion = "0.11.0-RC1"
  def scalacOptions = Seq("-deprecation", "-feature")
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
