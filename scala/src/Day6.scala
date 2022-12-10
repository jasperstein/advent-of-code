import scala.io.Source
object Day6 extends App {
    val input = Source.fromFile("inputs/day6.txt").getLines().next().toList
  def windows(input: List[Char], size: Int) = input.sliding(size)

  println(windows(input, 4).zipWithIndex.find(_._1.distinct.length == 4))
  println(windows(input, 14).zipWithIndex.find(_._1.distinct.length == 14))

}
