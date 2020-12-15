import scala.reflect.io.File

object Day15 extends App {
  private val input: String = File("Inputs/day15.txt").lines().next()

  private val startNumbers = input.split(",").map(_.toInt).zipWithIndex

  private val initial: Map[Int, Int] = startNumbers.init.toMap

  private def say(startNumbers: Array[(Int, Int)], initial: Map[Int, Int]): Iterator[(Int, Int)] = Iterator.unfold((startNumbers.last, initial)) {
    case arg@((lastSpoken, lastIdx), series) =>
      val nextNr = series.get(lastSpoken).map(lastIdx - _).getOrElse(0)
      Some(((nextNr, lastIdx + 1), ((nextNr, lastIdx + 1), series + (lastSpoken -> lastIdx))))
  }

  println(say(startNumbers, initial).find(_._2 == 2019))
  println(say(startNumbers, initial).find(_._2 == 29999999)) // Brute force FTW!
}
