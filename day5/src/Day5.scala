package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.Buffer

object Day5 extends AoCBase(5) {

  lazy val data: String = rawData.next

  def pair(c: Char) = if ( c.isUpper ) c.toLower else c.toUpper

  // Imperative version
  def reactI(s: String) = {
    val buf = s.toBuffer
    var hasChanged = true
    // Repeated passes over the polymer until no changes are occurring
    while ( hasChanged ) {
      hasChanged = false
      var i = 0
      // One pass over the polymer, reacting any pairs found
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

  // Just to see how this would perform, a functional version. This
  // isn't as clean as I might like, but does okay. It helps that we
  // don't need to reverse the list after each step of rebuilding it.
  // I'd estimate the slowdown as about 30% for problem 2, probably
  // from reconstructing as a String each time instead of staying a
  // List.
  def reactF(s: String) = {
    type L = List[Char]
    // One pass over the polymer, reacting any pairs found
    def reactPass(s: L, a: L, changed: Boolean): (L, Boolean) = s match {
      case x :: y :: t if x == pair(y) => reactPass(t, a, true)
      case x :: t => reactPass(t, x :: a, changed)
      case Nil => (a, changed)
    }
    // Repeated passes over the polymer until no changes are occurring
    def reactFully(s: L): L = {
      reactPass(s, Nil, false) match {
        case (r, true) => reactFully(r)
        case (r, false) => r
      }
    }
    reactFully(s.toList).mkString
  }

  def react(s: String) = reactI(s)

  def problem1() = {
    print(react(data).length)
  }

  def problem2() = {
    val base = react(data)
    val reagents = base.map(_.toLower).distinct
    val reagentLengths =
      for ( r <- reagents )
        yield react(base.filterNot(x => x == r || x == pair(r))).size
    print(reagentLengths.min)
  }

}
