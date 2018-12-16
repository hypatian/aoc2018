package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase

object Day16 extends AoCBase(16) {

  val beforePat = """^Before: \[(\d+), (\d+), (\d+), (\d+)\]$""".r
  val opPat = """^(\d+) (\d+) (\d+) (\d+)$""".r
  val afterPat = """^After:  \[(\d+), (\d+), (\d+), (\d+)\]$""".r

  def part1data = rawData.take(3235)
  def part2data = rawData.drop(3238)

  val befores: IndexedSeq[IndexedSeq[Int]] = (for {
    beforePat(r0, r1, r2, r3) <- part1data
  } yield IndexedSeq(r0.toInt, r1.toInt, r2.toInt, r3.toInt)).toIndexedSeq

  val ops: IndexedSeq[(Int, Int, Int, Int)] = (for {
    opPat(id, a, b, c) <- part1data
  } yield (id.toInt, a.toInt, b.toInt, c.toInt)).toIndexedSeq

  val afters: IndexedSeq[IndexedSeq[Int]] = (for {
    afterPat(r0, r1, r2, r3) <- part1data
  } yield IndexedSeq(r0.toInt, r1.toInt, r2.toInt, r3.toInt)).toIndexedSeq

  def problem1() = {
    val triples = (for {
      ((id, a, b, c), i) <- ops.zipWithIndex
      opcode <- Opcode.enumValues
      if Instr(opcode, a, b, c)(befores(i)) == afters(i)
    } yield i).groupBy(identity).values.map(_.size).filter(_ >= 3).size
    print(triples)
  }

  def solveOpcodes(): Map[Int, Opcode] = {
    var possibles =
      (for ( id <- 0 to 15 ) yield (id, Opcode.enumValues.toSet)).toMap
    for ( i <- ops.indices ) {
      val before = befores(i)
      val (id, a, b, c) = ops(i)
      val after = afters(i)
      val worked = (for {
        opcode <- possibles(id)
        if Instr(opcode, a, b, c)(before) == after
      } yield opcode)
      possibles = possibles.updated(id, worked)
    }
    while ( possibles.values.exists(_.size > 1) ) {
      for ( id <- possibles.keys ) {
        if ( possibles(id).size == 1 ) {
          possibles = possibles.map({ (k, s) =>
            if ( k == id ) (k, s) else (k, s -- possibles(id))
          })
        }
      }
    }
    possibles.mapValues(_.head)
  }

  def problem2() = {
    val opcodeOfId = solveOpcodes()
    var s = IndexedSeq(0, 0, 0, 0)
    for ( opPat(id, a, b, c) <- part2data ) {
      s = Instr(opcodeOfId(id.toInt), a.toInt, b.toInt, c.toInt)(s)
    }
    print(s(0))
  }

}
