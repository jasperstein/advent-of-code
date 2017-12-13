package day13

object Firewall extends App {

  def parseInput(input: String): Map[Int, Int] = {
    input.lines.toList
      .map(_.split(":").toList.map(_.trim))
      .map(parts => (Integer.parseInt(parts(0)), Integer.parseInt(parts(1))))
      .toMap
  }

  println(parseInput(Input.example))

  def severity(scanners: Map[Int, Int], step: Int): Int = {
    if (!scanners.contains(step)) {
      println(s"no entry for $step")
      0
    } else if (step % (2*scanners(step)-2) != 0) {
      println(s"Step $step: depth ${scanners(step)}, location ${step % (2*scanners(step) - 2)}")
      0
    }
    else {
      println(s"Hit! $step, depth ${scanners(step)}")
      step * scanners(step)
    }
  }

  println(Range(0,7).toList.map(severity(parseInput(Input.example), _)))

  val input = parseInput(Input.star1)
  println(Range(0,input.keys.max + 1).toList.map(severity(input, _)))
  println(Range(0,input.keys.max + 1).toList.map(severity(input, _)).sum)
}
