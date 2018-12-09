package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.annotation.tailrec
import scala.collection.mutable.{Buffer, IndexedSeq}

object Day9 extends AoCBase(9) {

  val pat = """(\d+) players; last marble is worth (\d+) points""".r

  private[this] val (players, maxMarble) = rawData.next match {
    case pat(players, maxMarble) => (players.toInt, maxMarble.toInt)
  }

  def game(p: Int, m: Int): Seq[Long] = {
    // In the style of Okasaki's purely functional queue
    // implementation[1], we have a "front" and a "back" list, and we
    // reverse them into each other as needed. I had the idea to use
    // this approach because I noted that we're consuming things
    // forward and backwards from the current position in relatively
    // small amounts, and as the size of the circle increases the
    // relative look-ahead/look-back decreases, so we shouldn't have
    // to reverse very often, and it should happen less often as time
    // goes on.
    //
    // Using a doubly-linked list would probably be a better choice
    // and simpler over-all, but this popped into my head and seemed
    // fun.
    //
    // [1] Chris Okasaki, "Simple and Efficient Purely Functional
    // Queues and Deques":
    // https://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf

    // The front list (head is the current value)
    // Start with two items in it to avoid having to deal with the
    // "there aren't two items in the whole collection" case later.
    var f: List[Int] = List(0, 1)

    // The back list (first item is the item before the current value, etc.)
    var b: List[Int] = List()

    val score = IndexedSeq.fill(players)(0L)

    // Since 0 and 1 are already in there, start with 2
    var n = 2
    while ( n <= m ) {
      if ( n % 23 != 0 ) {
        // This is where we don't have to deal with < 2 items total.
        // Check if we have enough to consume two things from f, else reverse
        if ( ! f.isDefinedAt(1) ) { f = f ++ b.reverse; b = Nil }
        // Copy two things from the front to the back
        b = f(1) :: f(0) :: b
        // Drop those things and throw the new value on the front
        f = n :: f.drop(2)
      } else {
        // Check if we have enough to consume seven things from b, else reverse
        if ( ! b.isDefinedAt(6) ) { b = b ++ f.reverse; f = Nil }
        // Add n and the seventh thing from the back to the player's score
        score((n - 1) % players) += n + b(6)
        // Copy six things from the back to the front
        f = b(5) :: b(4) :: b(3) :: b(2) :: b(1) :: b(0) :: f
        // Drop seven things from the back
        b = b.drop(7)
      }
      n += 1
    }
    score
  }

  def problem1() = print(game(players, maxMarble).max)
  def problem2() = print(game(players, maxMarble * 100).max)

}
