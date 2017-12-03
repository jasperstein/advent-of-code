package day3

import day0.Types.Position
import day3.PositionCalculator.{Memo, PosWithMemo}

import scala.collection.mutable

object SpiralMemory extends App {
  val origin = Position(0, 0)

  val field = mutable.Map[Position, Int](origin -> 1)

  var answerKnown = false

  def position(i: Int):Position = {
    val result = (1 until i).foldLeft(PosWithMemo(origin, Memo(0, 1))) {
      (posWithMemo: PosWithMemo, i: Int) =>
        val pos = posWithMemo.pos
        field(pos) = calculateFieldValue(pos)
        PositionCalculator.takeStep(posWithMemo)
    }
    result.pos
  }

  def calculateFieldValue(position: Position): Int = {
    if (position == origin) return 1

    val neighbours = List(position.left, position.up.left, position.up, position.up.right, position.right, position.down.right, position.down, position.down.left)
    val sum = neighbours.map(field.getOrElse(_, 0)).sum
    if (sum > 347991 && !answerKnown) {
      println(sum)
      answerKnown = true
    }
    sum
  }

  def taxicab(pos1: Position, pos2: Position) = Math.abs(pos1.x - pos2.x) + Math.abs(pos1.y - pos2.y)

  println(taxicab(origin, position(347991)))
}
