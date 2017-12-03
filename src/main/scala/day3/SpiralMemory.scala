package day3

import day0.Types.Position
import day3.PositionCalculator.{Memo, PosWithMemo}

object SpiralMemory extends App {
  val origin = Position(0, 0)

  def position(i: Int):Position = {
    val result = (1 until i).foldLeft(PosWithMemo(origin, Memo(0, 1))) {
      (posWithMemo: PosWithMemo, i: Int) => PositionCalculator.takeStep(posWithMemo)
    }
    result.pos
  }

  def taxicab(pos1: Position, pos2: Position) = Math.abs(pos1.x - pos2.x) + Math.abs(pos1.y - pos2.y)

  println(taxicab(origin, position(347991)))
}
