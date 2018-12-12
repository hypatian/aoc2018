package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.collection.mutable.Map

object Day12 extends AoCBase(12) {

  def dotToBools(dots: String): Seq[Boolean] = dots.map({
    case '#' => true
    case '.' => false
  })

  def boolsToDots(bools: Seq[Boolean]): String = bools.map({
    case true => '#'
    case false => '.'
  }).mkString

  trait Rules {
    val init: (Long, String)
    def trans(s: String): Char
  }

  object Test extends Rules {
    val init = (0, "#  # #  ##      ###   ###")
    def trans(s: String): Char = s match {
      case "   ##" => '#'
      case "  #  " => '#'
      case " #   " => '#'
      case " # # " => '#'
      case " # ##" => '#'
      case " ##  " => '#'
      case " ####" => '#'
      case "# # #" => '#'
      case "# ###" => '#'
      case "## # " => '#'
      case "## ##" => '#'
      case "###  " => '#'
      case "### #" => '#'
      case "#### " => '#'
      case _ => ' '
    }
  }

  object Real extends Rules {
    val init = (0, "##  ##    # # ####        ## # ##### ##  # #  # #   ## ##### ### ##   #    ##    #  ### #   # # # #")
    def trans(s: String): Char = s match {
      case "## # " => ' '
      case "## ##" => ' '
      case "#  ##" => ' '
      case "# # #" => ' '
      case "  #  " => '#'
      case "# ## " => ' '
      case "##   " => '#'
      case " #  #" => ' '
      case "# ###" => ' '
      case "     " => ' '
      case "   # " => '#'
      case "#  # " => '#'
      case "###  " => '#'
      case " #   " => '#'
      case "### #" => '#'
      case "#### " => ' '
      case " ## #" => '#'
      case "# #  " => '#'
      case " ### " => '#'
      case " # ##" => ' '
      case "#####" => '#'
      case "    #" => ' '
      case " ####" => ' '
      case " ##  " => '#'
      case "##  #" => ' '
      case "#   #" => ' '
      case "  ###" => '#'
      case "   ##" => ' '
      case "#    " => ' '
      case "  ## " => ' '
      case " # # " => '#'
      case "  # #" => '#'
    }
  }

  def norm(state: (Long, String)): (Long, String) = {
    val (offset, board) = state
    val leadingSpaceCount = board.takeWhile(_ == ' ').size
    (offset + leadingSpaceCount, board.trim)
  }

  def run(state: (Long, String), trans: String => Char): (Long, String) = {
    val (offset, board) = state
    norm((offset - 2, ("    " + board + "    ").sliding(5).map(trans).mkString))
  }

  def sumPots(state: (Long, String)): Long = {
    val (offset, board) = state
    board.zipWithIndex.map((v, i) => (v, i.toLong + offset)).filter((v, i) => v == '#').map((v, i) => i).sum
  }

  def problem1() = {
    var state = Real.init
    val trans = Real.trans
    for ( i <- 1 to 20 ) {
      state = run(state, trans)
    }
    println(sumPots(state))
    println(state)
  }

  def problem2() = {
    var state = Real.init
    val trans = Real.trans
    var i = 0L
    while ( i < 200L ) {
      state = run(state, trans)
      i += 1
    }
    // By visual inspection, this produces a glider pattern, which at
    // generation 200 is 102 spaces to the right of the origin,
    // travelling at a speed of c. So we can extrapolate the final
    // state as being the same pattern 50,000,000,000 spaces (less
    // 200) to the right of 102, and compute the desired sum at that
    // state.
    state = (102L + 50000000000L - 200L, "##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##")
    println(sumPots(state))
    println(state)
  }

}
