package day3

import day0.Types.Position

object Types {

  sealed trait Direction

  case object Left extends Direction

  case object Right extends Direction

  case object Up extends Direction

  case object Down extends Direction

  implicit class Day3Position(position: Position) {
    def go(direction: Direction): Position = direction match {
      case Left => position.left
      case Right => position.right
      case Up => position.up
      case Down => position.down
    }

  }

  case class Leg(length: Int, direction: Direction)
}