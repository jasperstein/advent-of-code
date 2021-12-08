import scala.io.Source
object Day5 extends App {
  val input = Source.fromFile("Inputs/day5.txt").getLines.map(_.split(" -> ").flatMap(_.split(",")).map(Integer.parseInt).toList).toList
  
  def isOrtho(coords: List[Int]) = coords(0) == coords(2) || coords(1) == coords(3)
  val (ortho, diag) = input.partition(isOrtho)

  val orthoposs = ortho.flatMap(coords => 
      (Math.min(coords(0),coords(2)) to Math.max(coords(0),coords(2))).flatMap(x => 
          (Math.min(coords(1), coords(3)) to Math.max(coords(1), coords(3))).map(y => (x,y))))

  val diagposs = diag.flatMap(coords => {
      val left = Math.min(coords(0), coords(2))
      val direction = Math.signum((coords(2) - coords(0))*(coords(3)-coords(1))).toInt
      val y0 = if (direction > 0) Math.min(coords(1), coords(3)) else Math.max(coords(1), coords(3))
      (0 to Math.max(coords(0), coords(2)) - left).map { i =>
          (left + i, y0 + i * direction)
      }
    })

  val counts = (orthoposs++diagposs).foldLeft(Map[(Int, Int), Int]().withDefaultValue(0)) { 
    case (m, coord) =>
        m.updated(coord, m(coord) + 1)
    }
  println(counts.toList.filter(_._2 > 1).length)
}
