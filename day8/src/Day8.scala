package org.hypatian.demo.aoc2018

import org.hypatian.demo.aoc.AoCBase
import scala.annotation.tailrec
import scala.language.implicitConversions

object Day8 extends AoCBase(8) {

  case class Node(children: Seq[Node], metadata: Seq[Int]) {
    def metadataSum: Int = metadata.sum + children.map(_.metadataSum).sum
    def value: Int =
      if ( children.size == 0 ) metadata.sum
      else metadata.map(i => children.lift(i - 1)).flatten.map(_.value).sum
  }
  object Node {
    def parse(n: Int, input: Seq[Int], nodes: Seq[Node]): (Seq[Int], Seq[Node]) = n match {
      case 0 => (input, nodes)
      case n =>
        val Seq(nChildren, nMetadata, rem1: _*) = input
        val (rem2, children) = parse(nChildren, rem1, Seq.empty)
        parse(n - 1, rem2.drop(nMetadata),
          nodes :+ Node(children, rem2.take(nMetadata)))
    }
    def apply(input: Seq[Int]): Node = parse(1, input, Seq.empty) match {
      case (Seq(), Seq(node)) => node
    }
  }

  val data = Node(rawData.next.split(' ').toSeq.map(_.toInt))

  def problem1() = print(data.metadataSum)

  def problem2() = print(data.value)

}
