package day0

object Types {

  case class Position(x: Int, y: Int) {
    def left: Position = Position(x - 1, y)
    def right: Position = Position(x + 1, y)
    def up: Position = Position(x, y + 1)
    def down: Position = Position(x, y - 1)
  }

  case class State(pos: Position, markers: Map[Marker, Set[Position]])
}