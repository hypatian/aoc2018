package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.annotation.tailrec

object Day9 extends AoCBase(9) {

  val pat = """(\d+) players; last marble is worth (\d+) points""".r

  class State(players: Int) {
    import scala.collection.mutable.{Buffer, IndexedSeq}
    val board = Buffer(0)
    val score = IndexedSeq.fill(players)(0)
    var current = 0
    def play(n: Int): Unit = {
      if ( n % 23 != 0 ) {
        current = (current + 2) % board.size
        board.insert(current, n)
      } else {
        current = (((current - 7) % board.size) + board.size) % board.size
        score((n - 1) % players) += n + board.remove(current)
        current = current % board.size
      }
    }
  }
  object State {
    def apply(players: Int): State = new State(players)
  }

  val (players, maxMarble) = rawData.next match {
    case pat(players, maxMarble) => (players.toInt, maxMarble.toInt)
  }

  def problem1() = {
    val state = State(players)
    for ( n <- 1 to maxMarble ) state.play(n)
    print(state.score.max)
  }

  def problem2() = {
    val state = State(players)
    for ( n <- 1 to (maxMarble * 100) ) state.play(n)
    print(state.score.max)
  }

}
