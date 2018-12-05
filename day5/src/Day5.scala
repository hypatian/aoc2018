package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.Buffer

object Day5 extends AoCBase(5) {

  lazy val data: String = rawData.next

  def pair(c: Char) = if ( c.isUpper ) c.toLower else c.toUpper

  def react(s: String): String = {
    val buf = s.toBuffer
    var hasChanged = true
    while ( hasChanged ) {
      hasChanged = false
      var i = 0
      while ( i < buf.size - 1 ) {
        if ( buf(i) == pair(buf(i + 1)) ) {
          buf.remove(i, 2)
          hasChanged = true
        } else {
          i += 1
        }
      }
    }
    buf.mkString
  }

  def problem1() = {
    print(react(data).length)
  }

  def problem2() = {
    // With buf pre-reduced by problem1
    val base = react(data)
    val reagents = base.map(_.toLower).distinct
    val reagentLengths =
      for ( r <- reagents ) yield {
        var u = r.toUpper
        react(base.filterNot(x => x == r || x == u)).size
      }
    print(reagentLengths.min)
  }

}
