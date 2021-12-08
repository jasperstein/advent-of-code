import scala.io.Source
object Day6 extends App {
  val input = Source.fromFile("Inputs/day6.txt").getLines.next.split(",").map(Integer.parseInt).map(_.toLong).toList
  
  val initial = input.foldLeft(Map[Long,Long]().withDefaultValue(0L)) { case (m, i) => m.updated(i, m(i) + 1) }

  def nextDay(fish: Map[Long,Long]) = Map(
      0L -> fish(1),
      1L -> fish(2),
      2L -> fish(3),
      3L -> fish(4),
      4L -> fish(5),
      5L -> fish(6),
      6L -> (fish(7) + fish(0)),
      7L -> fish(8),
      8L -> fish(0)
  )

  def result(days: Int) = (1 to days).foldLeft(initial) { case (fish, _) => nextDay(fish) }
  println(result(80))
  println(result(80).values)
  println(result(80).values.sum)
  println(result(256))
  println(result(256).values)
  println(result(256).values.sum)
}
