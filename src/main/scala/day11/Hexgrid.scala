package day11

object Hexgrid extends App {
  def input2Directions(input:String): List[Direction] = input.split(",").toList.map {
    case "ne" => NE
    case "n" => N
    case "nw" => NW
    case "se" => SE
    case "s" => S
    case "sw" => SW
  }

  def countRawSteps(steps: List[Direction]): Map[Direction, Int] = {
    var result = Map[Direction, Int]()
    steps.foreach(d => result += d -> (result.getOrElse(d, 0) + 1))
    result
  }

  def totalSteps(steps: Map[Direction, Int]): Int = {
    var n = steps.getOrElse(N,0) - steps.getOrElse(S,0)
    var ne = steps.getOrElse(NE, 0) - steps.getOrElse(SW, 0)
    var nw = steps.getOrElse(NW, 0) - steps.getOrElse(SE, 0)
    if (ne > 0 && nw > 0) {
      val min = Math.min(ne, nw)
      n += min
      ne -= min
      nw -= min
    } else if (ne < 0 && nw < 0) {
      val min = Math.min(Math.abs(ne), Math.abs(nw))
      n -= min
      ne += min
      nw += min
    }
    if (n * (ne+nw) < 0)
      Math.abs(Math.max(n, ne + nw))
    else
      Math.abs(n + ne + nw)
  }

  import day11.Input._

  println(totalSteps(countRawSteps(input2Directions(example1))))
  println(totalSteps(countRawSteps(input2Directions(example2))))
  println(totalSteps(countRawSteps(input2Directions(example3))))
  println(totalSteps(countRawSteps(input2Directions(example4))))

  println(totalSteps(countRawSteps(input2Directions(star1))))

}
