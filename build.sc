import mill._, scalalib._

trait AocModule extends ScalaModule {
  def scalaVersion = "0.11.0-RC1"
  def scalacOptions = Seq("-deprecation", "-feature")
}

object day1 extends Module {
  object problem1 extends AocModule
  object problem2 extends AocModule
}
