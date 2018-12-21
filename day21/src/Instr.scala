package org.hypatian.demo.aoc2018

import Opcode._

case class Instr(op: Opcode, a: Long, b: Long, c: Long) {
  def apply(r: IndexedSeq[Long]): IndexedSeq[Long] = op match {
    case Opcode.addr => r.updated(c.toInt, r(a.toInt) + r(b.toInt))
    case Opcode.addi => r.updated(c.toInt, r(a.toInt) + b)
    case Opcode.mulr => r.updated(c.toInt, r(a.toInt) * r(b.toInt))
    case Opcode.muli => r.updated(c.toInt, r(a.toInt) * b)
    case Opcode.banr => r.updated(c.toInt, r(a.toInt) & r(b.toInt))
    case Opcode.bani => r.updated(c.toInt, r(a.toInt) & b)
    case Opcode.borr => r.updated(c.toInt, r(a.toInt) | r(b.toInt))
    case Opcode.bori => r.updated(c.toInt, r(a.toInt) | b)
    case Opcode.setr => r.updated(c.toInt, r(a.toInt))
    case Opcode.seti => r.updated(c.toInt, a)
    case Opcode.gtir => r.updated(c.toInt, if ( a > r(b.toInt) ) 1L else 0L)
    case Opcode.gtri => r.updated(c.toInt, if ( r(a.toInt) > b ) 1L else 0L)
    case Opcode.gtrr => r.updated(c.toInt, if ( r(a.toInt) > r(b.toInt) ) 1L else 0L)
    case Opcode.eqir => r.updated(c.toInt, if ( a == r(b.toInt) ) 1L else 0L)
    case Opcode.eqri => r.updated(c.toInt, if ( r(a.toInt) == b ) 1L else 0L)
    case Opcode.eqrr => r.updated(c.toInt, if ( r(a.toInt) == r(b.toInt) ) 1L else 0L)
  }
}
