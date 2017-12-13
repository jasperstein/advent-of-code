package day13

object Firewall extends App {

  def parseInput(input: String): Map[Int, Int] = {
    input.lines.toList
      .map(_.split(":").toList.map(_.trim))
      .map(parts => (Integer.parseInt(parts(0)), Integer.parseInt(parts(1))))
      .toMap
  }

  println(parseInput(Input.example))

  def severity(scanners: Map[Int, Int], depth: Int, delay: Int = 0): Int = {
    if (!scanners.contains(depth) || (depth + delay) % (2*scanners(depth)-2) != 0) 0
    else depth * scanners(depth)
  }

  private val exampleInput: Map[Int, Int] = parseInput(Input.example)
  println(Range(0,7).toList.map(severity(exampleInput, _)))

  val input = parseInput(Input.star1)
  println(Range(0,input.keys.max + 1).toList.map(severity(input, _)))
  println(Range(0,input.keys.max + 1).map(severity(input, _)).sum)

  def delaySeverity(delay: Int) = Range(0, input.keys.max + 1).map(severity(input, _, delay)).sum

  var found = false
  var delay = -1
  while (!found) {
    delay = delay + 1
    val score = delaySeverity(delay)
    if (score == 0 && (delay % (2*input(0) - 2) != 0)) found = true
  }
  println(delay)
}
