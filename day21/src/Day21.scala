package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day21 extends AoCBase(21) {

  val ipPat = """^#ip (\d)$""".r
  val opPat = """^([a-z]+) (\d+) (\d+) (\d+)$""".r

  val ipRegister = rawData.next match {
    case ipPat(r) => r.toInt
  }

  val program = rawData.toSeq.drop(1).map({
    case opPat(opcode, a, b, c) =>
      Instr(Opcode.enumValueNamed(opcode), a.toLong, b.toLong, c.toLong)
  }).toIndexedSeq

  def exec1(r: IndexedSeq[Long]): IndexedSeq[Long] = {
    var rn = program(r(ipRegister).toInt)(r)
    //println(s"ip=${r(ipRegister)} $r ${program(r(ipRegister))} $rn")
    rn
  }

  def execAll(p1Stop: Boolean, ir: IndexedSeq[Long]): Long = {
    var seen = Set.empty[Long]
    var previous = 0L
    var n = 0L
    var r = ir
    var ip = 0L
    while ( program.isDefinedAt(ip.toInt) ) {
      r = exec1(r.updated(ipRegister, ip))
      if ( ip == 28 ) {
        if ( p1Stop ) return r(5)
        if ( seen(r(5)) ) return previous
        previous = r(5)
        seen += previous
      }
      ip = r(ipRegister)
      ip += 1
      n += 1
    }
    -1L
  }

  def problem1() =
    print(execAll(true, IndexedSeq(0L,0L,0L,0L,0L,0L)))

  def problem2() =
    print(execAll(false, IndexedSeq(0L,0L,0L,0L,0L,0L)))

}
