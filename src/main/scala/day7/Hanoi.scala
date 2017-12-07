package day7

object Hanoi extends App {

  var weights = Map[String, Int]() // we might just need these later...

  var parents = Map[String, String]()

  def parseWeight(spec: String): ((String, Int)) = {
    val split = spec.split("\\(").map(_.trim)
    val progName = split(0)
    val weight = Integer.parseInt(split(1).init)
    progName -> weight
  }

  def parseLine(line: String): Unit = {
    val elems = line.split("->").map(_.trim)
    val progWeight = parseWeight(elems(0))
    weights += progWeight
    val parent: String = progWeight._1
    if (elems.length > 1) {
      for (child <- elems(1).split(",").map(_.trim)) {
        parents += child -> parent
      }
    }
  }

  def run(input: String): Unit = {
    weights = Map()
    parents = Map()
    input.lines.foreach(parseLine)
    println(weights.keySet diff parents.keySet)
  }

  run(Input.example1)
  run(Input.star1)

}
