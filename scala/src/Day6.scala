import scala.reflect.io.File

object Day6 extends App {
  private val input: List[String] = File("Inputs/day6.txt").lines().toList

  private def answers = sets(Set(), curr => chars => curr ++ Set(chars: _*))
  private def answers2 = sets(Set("abcdefghijklmnopqrstuvwxyz".toCharArray: _*), curr => chars => curr.intersect(Set(chars: _*)))

  private def sets(initial: Set[Char], combine: Set[Char] => Array[Char] => Set[Char]) = input.foldLeft(
    (initial, List[Set[Char]]())
  )(
    { case ((curr, acc), line) =>
      if (line.isBlank) (initial, curr :: acc)
      else (combine(curr)(line.toCharArray), acc)
    }
  )

  println(answers._2.map(_.size).sum + answers._1.size)
  println(answers2._2.map(_.size).sum + answers2._1.size)
}
