package day12

import scala.collection.{immutable, mutable}

object day12 extends App {

  def rules = day12Input.rules.lines.filter(_.endsWith("#")).map(_.takeWhile(_ != ' ')).toList

  println(rules)

//  case class Generation(i: Int) extends AnyVal
  case class Position(i: Long) extends AnyVal

  var pots = mutable.Map[Position, Char]().withDefaultValue('.')

  val zipped: immutable.Seq[(Char, Int)] = day12Input.initial.zipWithIndex
  for (entry <- zipped) {
    pots(Position(entry._2)) = entry._1
  }

//  println(pots.toList.sortBy(_._1._2.i))
  // #.#.#....##...##...##...#.##.#.###...#.##...#....#.#...#.##.........#.#...#..##.#.....#..#.###

  val seen = new java.util.Stack[String]()

  def toStateString(nextPots: mutable.Map[Position, Char], min: Long, max: Long): String = {
    (min.toInt to max.toInt).map(i => nextPots(Position(i))).mkString
  }

  def detectCycles(nextPots: mutable.Map[Position, Char], min: Long, max: Long): Int = {
    val state = toStateString(nextPots, min, max)
    val idx = seen.indexOf(state)
    seen.push(state)
    idx
  }

  var prevMinPos = 0L
  var prevMaxPos = day12Input.initial.length.toLong
  var i: Long = 1
  while (i <= 200L) {
//    if (i % 1000000000 == 0) { println(i) }
    var minPos = Long.MaxValue
    var maxPos = Long.MinValue
    var nextPots = mutable.Map[Position, Char]().withDefaultValue('.')
    for (p <- prevMinPos - 2 to prevMaxPos + 2) {
      val prev = List(
        pots(Position(p-2)),
        pots(Position(p-1)),
        pots(Position(p)),
        pots(Position(p+1)),
        pots(Position(p+2))
      ).mkString
      if (rules.contains(prev)) {
        nextPots(Position(p)) = '#'
        if (p < minPos) minPos = p
        if (p > maxPos) maxPos = p
      }
    }
    val cycle = detectCycles(nextPots, minPos, maxPos)
//    if (cycle > -1) {
//      println(i, cycle)
//    }
    prevMaxPos = maxPos
    prevMinPos = minPos
    pots = nextPots
    i = i+1
    println(toStateString(nextPots, minPos, maxPos))
  }

  println(pots.toList.filter(p => p._2 == '#').map(p => p._1.i).sum)
  println(pots.toList.filter(p => p._2 == '#').sortBy(_._1.i))
// List((Position(111),#), (Position(116),#), (Position(122),#), (Position(129),#), (Position(134),#), (Position(139),#), (Position(145),#), (Position(153),#), (Position(161),#), (Position(169),#), (Position(174),#), (Position(179),#), (Position(184),#), (Position(192),#), (Position(200),#), (Position(209),#), (Position(219),#), (Position(224),#), (Position(230),#), (Position(238),#))
//  200 -> 4508
//  201 -> 4528
  println(508 + 20*50000000000L)
}
