package org.hypatian.demo.aoc2018

case class Mob(var p: Position, val k: Kind, val ap: Int = 3) {
  var hp: Int = 200
  def stats: String = s"${k.toChar}(${hp})"
}
object Mob {
  implicit val positionOrdering: Ordering[Mob] = Ordering.by(m => m.p)
  val hpOrdering: Ordering[Mob] = Ordering.fromLessThan { (a: Mob, b: Mob) =>
    if ( a.hp < b.hp ) true
    else if ( a.hp > b.hp ) false
    else if ( a.p.y < b.p.y ) true
    else if ( a.p.y > b.p.y ) false
    else ( a.p.x < b.p.x )
  }
}
