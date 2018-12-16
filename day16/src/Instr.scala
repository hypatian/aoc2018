package org.hypatian.demo.aoc2018

import Opcode._

case class Instr(op: Opcode, a: Int, b: Int, c: Int) {
  def apply(r: IndexedSeq[Int]): IndexedSeq[Int] = op match {
    case Opcode.addr => r.updated(c, r(a) + r(b))
    case Opcode.addi => r.updated(c, r(a) + b)
    case Opcode.mulr => r.updated(c, r(a) * r(b))
    case Opcode.muli => r.updated(c, r(a) * b)
    case Opcode.banr => r.updated(c, r(a) & r(b))
    case Opcode.bani => r.updated(c, r(a) & b)
    case Opcode.borr => r.updated(c, r(a) | r(b))
    case Opcode.bori => r.updated(c, r(a) | b)
    case Opcode.setr => r.updated(c, r(a))
    case Opcode.seti => r.updated(c, a)
    case Opcode.gtir => r.updated(c, if ( a > r(b) ) 1 else 0)
    case Opcode.gtri => r.updated(c, if ( r(a) > b ) 1 else 0)
    case Opcode.gtrr => r.updated(c, if ( r(a) > r(b) ) 1 else 0)
    case Opcode.eqir => r.updated(c, if ( a == r(b) ) 1 else 0)
    case Opcode.eqri => r.updated(c, if ( r(a) == b ) 1 else 0)
    case Opcode.eqrr => r.updated(c, if ( r(a) == r(b) ) 1 else 0)
  }
}
