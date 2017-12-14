package day14

object Defrag extends App {

  val input = "xlqgujun"

  val ones = Map('0' -> 0, '1' -> 1, '2' -> 1, '3' -> 2, '4' -> 1, '5' -> 2, '6' -> 2, '7' -> 3,
                 '8' -> 1, '9' -> 2, 'a' -> 2, 'b' -> 3, 'c' -> 2, 'd' -> 3, 'e' -> 3, 'f' -> 4)

  val hashes: List[String] = Range(0, 128).map(input + "-" + _).map(day10.Hash.hash).toList

  val sums = hashes.map(i => i.map(ones(_)).sum)
  println(sums.sum)

}
