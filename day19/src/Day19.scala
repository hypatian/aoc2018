package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day19 extends AoCBase(19) {

  val ipPat = """^#ip (\d)$""".r
  val opPat = """^([a-z]+) (\d+) (\d+) (\d+)$""".r

  val ipRegister = rawData.next match {
    case ipPat(r) => r.toInt
  }

  val program = rawData.toSeq.drop(1).map({
    case opPat(opcode, a, b, c) =>
      Instr(Opcode.enumValueNamed(opcode), a.toInt, b.toInt, c.toInt)
  }).toIndexedSeq

  def exec1(r: IndexedSeq[Int]): IndexedSeq[Int] = {
    var rn = program(r(ipRegister))(r)
    //println(s"ip=${r(ipRegister)} $r ${program(r(ipRegister))} $rn")
    rn
  }

  def execAll(ir: IndexedSeq[Int]): IndexedSeq[Int] = {
    var n = 0
    var r = ir
    var ip = 0
    while ( program.isDefinedAt(ip) ) {
      r = exec1(r.updated(ipRegister, ip))
      ip = r(ipRegister)
      ip += 1
      n += 1
    }
    r
  }

  def problem1() = {
    val r = execAll(IndexedSeq(0,0,0,0,0,0))
    print(r(0))
  }

  def problem2() = {
    // It turns out, this is the sum of all of the factor pairs.
    // By hand decompiling the code, the meaning is more or less:
    //
    // var r0 = 0
    // for ( r1 <- 1 to r4 )
    //   for ( r3 <- 1 to r4 )
    //     if ( r1 * r3 == r4 ) r0 += r1
    //
    // This is not so bad when the input is 875 (as it is in
    // problem 1), but in problem 2 it gets a bit... trickier. Because
    // before the loop starts for problem 2, r4 is set to 10551275.
    //
    // So, that's painful.
    //
    // The factor pairs of 10551275 are:
    val factorPairs = Seq(
      (1, 10551275), (5, 2110255), (7, 1507325), (25, 422051), (35, 301465),
      (175, 60293)
    )
    val sum = factorPairs.map(_._1).sum + factorPairs.map(_._2).sum
    print(sum)
  }

}
