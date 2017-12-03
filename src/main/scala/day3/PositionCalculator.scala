package day3

import day0.Types.Position
import day3.Types._

object PositionCalculator {
  import Types.Day3Position

  case class Memo(legNr: Int, stepOfLeg: Int)

  def directions = List(Right, Up, Left, Down)

  def leg(i: Int) = Leg(i/2 + 1, directions(i % 4))

  case class PosWithMemo(pos: Position, memo: Memo)

  def takeStep(posWithMemo: PosWithMemo): PosWithMemo = {
    val memo = posWithMemo.memo
    val position = posWithMemo.pos
    val currLeg = leg(memo.legNr)
//    println(s"I am at $position, leg ${memo.legNr} ($currLeg) step ${memo.stepOfLeg}")
    val isLastStepOfLeg = memo.stepOfLeg >= currLeg.length
    val nextMemo = if (isLastStepOfLeg) Memo(memo.legNr + 1, 1) else Memo(memo.legNr, memo.stepOfLeg + 1)
    val nextPos = position.go(currLeg.direction)
//    println(s"Next: $nextPos, $nextMemo")
    PosWithMemo(position.go(currLeg.direction), nextMemo)
  }
}
