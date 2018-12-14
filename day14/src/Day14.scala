package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.Buffer

object Day14 extends AoCBase(14) {

  val data = 635041

  var recipes = Buffer[Byte](3, 7)
  var elf = Array(0, 1)

  inline def tick(): Unit = {
    val e0 = recipes(elf(0))
    val e1 = recipes(elf(1))
    val n = e0 + e1
    if ( n < 10 ) {
      recipes += n.toByte
    } else {
      recipes += (n / 10).toByte
      recipes += (n % 10).toByte
    }
    elf(0) = (elf(0) + e0 + 1) % recipes.size
    elf(1) = (elf(1) + e1 + 1) % recipes.size
  }

  inline def tick10k(): Unit = {
    var i = 0
    while ( i < 10000 ) {
      tick()
      i += 1
    }
  }

  def problem1() = {
    while ( recipes.size < data + 10 ) tick()
    print(recipes.slice(data, data + 10).mkString(""))
  }

  def problem2() = {
    val target = data.toString.map(_.toString.toByte)
    var search = 0
    var found = -1
    while ( found == -1 ) {
      tick10k()
      found = recipes.indexOfSlice(target, search)
      search = recipes.size - target.size
    }
    print(found)
  }

}
