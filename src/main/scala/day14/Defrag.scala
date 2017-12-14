package day14

import scala.collection.immutable

object Defrag extends App {

  val input = "xlqgujun"
  val example = "flqrgnkx"

  lazy val ones = Map('0' -> 0, '1' -> 1, '2' -> 1, '3' -> 2, '4' -> 1, '5' -> 2, '6' -> 2, '7' -> 3,
    '8' -> 1, '9' -> 2, 'a' -> 2, 'b' -> 3, 'c' -> 2, 'd' -> 3, 'e' -> 3, 'f' -> 4)

  val hashes: List[String] = Range(0, 128).map(input + "-" + _).map(day10.Hash.hash).toList

  val sums = hashes.map(i => i.map(ones(_)).sum)
  println(sums.sum)

  import day10.Hash.{dense, sparse}

  def rowBits(input: String, row: Int) = dense(sparse(input + "-" + row).hash)
    .map(byte => ("00000000" + byte.toBinaryString).takeRight(8)).zipWithIndex.flatMap({
    case (s, byteNr) => s.zipWithIndex.toList.map({ case (c, i) => (c, i + 8 * byteNr) })
  })

  private val allRowBits: immutable.IndexedSeq[(List[(Char, Int)], Int)] = Range(0, 128).map(i => rowBits(input, i)).zipWithIndex

  val all1Bits = allRowBits.flatMap(x => {
    val row = x._2
    val thisRowBits: List[(Char, Int)] = x._1
    thisRowBits.map({case ((c, col)) => (c, (row, col))})
  }).filter(_._1 == '1').map(_._2).toList

  def neighbours(current: (Int, Int)): List[(Int, Int)] = List((current._1, current._2 + 1), (current._1, current._2 - 1), (current._1 + 1, current._2), (current._1 - 1, current._2))

  var groups = 0
  var leftOverBits = all1Bits
  while (leftOverBits.nonEmpty) {
    groups += 1
    var tainted = List(leftOverBits.head)
    while (tainted.nonEmpty) {
      val current = tainted.head
      val neighbors = neighbours(current).intersect(leftOverBits)
      tainted = (tainted.tail ++ neighbors).distinct
      leftOverBits = leftOverBits.diff(List(current))
    }
  }

  println(groups)


}
