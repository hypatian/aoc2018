package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day24 extends AoCBase(24) {

  enum Side {
    case Immune, Infection
    def short: String = this.toString.charAt(1).toUpper.toString()
  }
  import Side.{Immune, Infection}
  enum DamageType {
    case Cold, Fire, Radiation, Slashing, Bludgeoning
    def short: String = this.toString.charAt(0).toString
  }
  import DamageType.{Cold, Fire, Radiation, Slashing, Bludgeoning}

  class Group(
    val id: Int,
    val side: Side,
    var units: Int, val hp: Int,
    val immune: Set[DamageType], val weak: Set[DamageType],
    val attackDamage: Int, val attackType: DamageType,
    val initiative: Int)
  {
    def isAlive: Boolean = units > 0
    def effectivePower: Int = units * attackDamage
    def multiplierAgainst(opp: Group): Int =
      if ( opp.immune(attackType) ) 0
      else if ( opp.weak(attackType) ) 2
      else 1
    def canDamage(opp: Group): Boolean = multiplierAgainst(opp) != 0

    def attack(opp: Group): Unit = if ( isAlive ) {
      assert(opp.isAlive)
      val damage = effectivePower * multiplierAgainst(opp)
      val slain = (damage / opp.hp) min (opp.units)
      opp.units -= slain
    }

    override def toString(): String = s"$side $id"
    def status: String = s"$side $id contains $units units ($effectivePower ${attackType}) Immune: ${immune.mkString(" ")}, Weak: ${weak.mkString(" ")}"
  }
  object Group {
    def apply(id: Int, side: Side, units: Int, hp: Int,
      immune: Set[DamageType], weak: Set[DamageType],
      attackDamage: Int, attackType: DamageType, initiative: Int): Group =
      new Group(id, side, units, hp, immune, weak, attackDamage, attackType, initiative)
  }

  abstract class Config {
    val groups: Seq[Group]
    def liveGroups = groups.filter(_.isAlive).toSeq
    def fighting: Boolean =
      (liveGroups.count(_.side == Immune) > 0 &&
        liveGroups.count(_.side == Infection) > 0)

    val targetingOrder: Ordering[Group] =
      Ordering.by((g: Group) => (g.effectivePower, g.initiative)).reverse

    def targetOrder(g: Group): Ordering[Group] =
      Ordering.by((o: Group) =>
        (g.multiplierAgainst(o), o.effectivePower, o.initiative)).reverse

    val attackingOrder: Ordering[(Group, Group)] =
      Ordering.by((at: (Group, Group)) => at._1.initiative).reverse

    def tick(): Unit = {
      var available = liveGroups
      val attacks = for {
        group <- liveGroups.sorted(targetingOrder)
        target <- (
          available.filter(t => t.side != group.side && group.canDamage(t)).
            sorted(targetOrder(group))).take(1)
        () = (available = available.filterNot(_ == target))
      } yield (group, target)
      for ( (attacker, target) <- attacks.sorted(attackingOrder) ) {
        attacker.attack(target)
      }
    }

    def unitCount: Int = groups.map(_.units).sum
    def immuneCount: Int = groups.filter(_.side == Immune).map(_.units).sum
    def infectionCount: Int = groups.filter(_.side == Infection).map(_.units).sum
  }

  def realConfig(boost: Int = 0): Config = new Config {
    val groups = Seq(
      Group(1, Immune, 8808, 5616, Set(Cold), Set(Radiation), 5 + boost, Bludgeoning, 10),
      Group(2, Immune, 900, 13511, Set.empty, Set(Radiation), 107 + boost, Radiation, 20),
      Group(3, Immune, 581, 10346, Set(Slashing), Set(Radiation), 140 + boost, Fire, 14),
      Group(4, Immune, 57, 9991, Set(Slashing, Radiation, Fire), Set(Bludgeoning), 1690 + boost, Fire, 4),
      Group(5, Immune, 4074, 6549, Set.empty, Set(Fire), 15 + boost, Radiation, 2),
      Group(6, Immune, 929, 5404, Set(Bludgeoning, Radiation), Set.empty, 45 + boost, Fire, 16),
      Group(7, Immune, 2196, 3186, Set(Radiation), Set(Fire), 10 + boost, Fire, 11),
      Group(8, Immune, 4420, 9691, Set(Fire), Set(Radiation), 21 + boost, Fire, 7),
      Group(9, Immune, 3978, 2306, Set.empty, Set(Cold, Radiation), 4 + boost, Fire, 12),
      Group(10, Immune, 1284, 4487, Set.empty, Set(Radiation, Bludgeoning), 32 + boost, Slashing, 19),


      Group(1, Infection, 4262, 23427, Set(Fire), Set(Slashing), 9, Slashing, 8),
      Group(2, Infection, 217, 9837, Set.empty, Set(Bludgeoning), 73, Bludgeoning, 1),
      Group(3, Infection, 5497, 33578, Set.empty, Set(Radiation, Cold), 11, Slashing, 17),
      Group(4, Infection, 866, 41604, Set.empty, Set(Cold), 76, Radiation, 15),
      Group(5, Infection, 1823, 19652, Set.empty, Set(Fire, Cold), 20, Slashing, 13),
      Group(6, Infection, 2044, 23512, Set.empty, Set(Cold), 22, Slashing, 9),
      Group(7, Infection, 373, 40861, Set(Cold), Set.empty, 215, Slashing, 18),
      Group(8, Infection, 5427, 43538, Set(Radiation), Set(Bludgeoning), 15, Slashing, 5),
      Group(9, Infection, 3098, 19840, Set.empty, Set(Bludgeoning, Cold), 12, Radiation, 3),
      Group(10, Infection, 785, 14669, Set.empty, Set.empty, 30, Fire, 6)
    )
  }

  def testConfig(boost: Int = 0): Config = new Config {
    val groups = Seq(
      Group(1, Immune, 17, 5390, Set.empty, Set(Radiation, Bludgeoning), 4507 + boost, Fire, 2),
      Group(2, Immune, 989, 1274, Set(Fire), Set(Bludgeoning, Slashing), 25 + boost, Slashing, 3),

      Group(1, Infection, 801, 4706, Set.empty, Set(Radiation), 116, Bludgeoning, 1),
      Group(2, Infection, 4485, 2961, Set(Radiation), Set(Fire, Cold), 12, Slashing, 4)
    )
  }

  def problem1() = {
    val config = realConfig()
    while ( config.fighting ) config.tick()
    print(config.liveGroups.map(_.units).sum)
  }

  def problem2(): Unit =
    for ( boost <- Iterator.from(1) ) {
      val config = realConfig(boost)
      var lastUnitCount = Int.MaxValue
      var damageDone = true
      while ( config.fighting && damageDone ) {
        config.tick()
        if ( lastUnitCount == config.unitCount ) damageDone = false
        lastUnitCount = config.unitCount
      }
      if ( damageDone && config.immuneCount > 0 ) {
        // Fighting was still going on, and immune system won
        print(config.immuneCount)
        return ()
      }
      // Otherwise, go up one
    }

}
