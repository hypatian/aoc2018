package org.hypatian.demo.aoc2018

enum Kind {
  case Elf, Goblin
  def unary_! : Kind = this match {
    case Elf => Goblin
    case Goblin => Elf
  }
  def toChar: Char = this match {
    case Elf => 'E'
    case Goblin => 'G'
  }
}
object Kind {
  def apply(c: Char): Kind = c match {
    case 'E' => Elf
    case 'G' => Goblin
  }
}
