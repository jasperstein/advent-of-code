package day12

import scala.collection.{immutable, mutable}

object day12 extends App {

  def rules = day12Input.rules.lines.filter(_.endsWith("#")).map(_.takeWhile(_ != ' ')).toList

  println(rules)

  case class Generation(i: Int) extends AnyVal
  case class Position(i: Int) extends AnyVal

  val pots = mutable.Map[(Generation, Position), Char]().withDefaultValue('.')

  val zipped: immutable.Seq[(Char, Int)] = day12Input.initial.zipWithIndex
  for (entry <- zipped) {
    pots((Generation(0), Position(entry._2))) = entry._1
  }

//  println(pots.toList.sortBy(_._1._2.i))
  // #.#.#....##...##...##...#.##.#.###...#.##...#....#.#...#.##.........#.#...#..##.#.....#..#.###

  for (i <- 1 to 20) {
    for (p <- -(2*i) to day12Input.initial.length + (2*i)) {
      val prev = List(
        pots((Generation(i-1), Position(p-2))),
        pots((Generation(i-1), Position(p-1))),
        pots((Generation(i-1), Position(p))),
        pots((Generation(i-1), Position(p+1))),
        pots((Generation(i-1), Position(p+2)))
      ).mkString
      val nextChar = if (rules.contains(prev)) '#' else '.'
      pots((Generation(i), Position(p))) = nextChar
    }
  }

  println(pots.toList.filter(p => p._1._1.i == 20 && p._2 == '#').map(p => p._1._2.i).sum)

}
