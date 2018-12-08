package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.annotation.tailrec
import scala.language.implicitConversions

object Day8 extends AoCBase(8) {

  val data = Node(rawData.next.split(' ').toSeq.map(_.toInt))

  case class Node(children: Seq[Node], metadata: Seq[Int]) {
    def metadataSum: Int = metadata.sum + children.map(_.metadataSum).sum
    def value: Int =
      if ( children.size == 0 ) metadata.sum
      else metadata.map(i => children.lift(i - 1)).flatten.map(_.value).sum
  }
  object Node {
    def parse(input: Seq[Int]): (Seq[Int], Node) = {
      @tailrec def loop(n: Int, input: Seq[Int], nodes: Seq[Node]):
          (Seq[Int], Seq[Node]) = n match {
        case 0 => (input, nodes)
        case n =>
          val (remainder, node) = parse(input)
          loop(n - 1, remainder, nodes :+ node)
      }
      val Seq(nChildren, nMetadata, rem1: _*) = input
      val (rem2, children) = loop(nChildren, rem1, Seq.empty)
      (rem2.drop(nMetadata), Node(children, rem2.take(nMetadata)))
    }
    def apply(input: Seq[Int]): Node = {
      val (remainder, node) = parse(input)
      assert(remainder.size == 0)
      node
    }
  }

  def problem1() = print(data.metadataSum)

  def problem2() = print(data.value)

}
