package day22

import day0.Types.Position
import day3.Types._

object Virus extends App {

  def parsePositions(input: String): Set[Position] = {
    val width = input.lines.toList.head.length
    val height = input.lines.size
    input.lines.zipWithIndex.flatMap(si =>
      si._1.zipWithIndex.filter(cj => cj._1 == '#')
        .map(cj => Position(cj._2 - (width/2), (height/2) - si._2))
    ).toSet
  }

  implicit class Rotate(direction: Direction) {
    def turnLeft(): Direction = direction match {
      case Left => Down
      case Right => Up
      case Up => Left
      case Down => Right
    }

    def turnRight(): Direction = direction.turnLeft().turnLeft().turnLeft()
  }

  def run(input: String, iterations: Int): Int = {
    var pos = Position(0, 0)
    var dir: Direction = Up
    var infected = parsePositions(input)
    var infCount = 0
    for (i <- Range(0, iterations)) {
      if (infected.contains(pos)) {
        dir = dir.turnRight()
        infected = infected - pos
      } else {
        dir = dir.turnLeft()
        infected = infected + pos
        infCount = infCount + 1
      }
      pos = pos.go(dir)
//      println(s"$pos $dir")
    }
    infCount
  }

  println(run(Input.example, 10000))
  println(run(Input.star1, 10000))

}
