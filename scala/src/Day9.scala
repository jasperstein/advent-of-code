import scala.annotation.tailrec
import scala.reflect.io.File

object Day9 extends App {
  private val input: List[Long] = File("Inputs/day9.txt").lines().map(java.lang.Long.parseLong).toList

  @tailrec
  def find(target: Long, l: List[Long]): Either[Unit, (Long, Long)] =
    l match {
      case Nil => Left("Not found")
      case ::(head, Nil) => Left("Not found")
      case ::(head, next) =>
        val lastInt = next.last
        val sum = head + lastInt
        val compared = sum.compareTo(target)
        if (compared == 0) Right((head, lastInt))
        else if (compared < 0) find(target, next)
        else find(target, l.init)
    }

  private val part1: Option[List[Long]] = input.sliding(26).find(window => find(window.last, window.init.sorted).isLeft)

  println(part1)

  val target = part1.getOrElse(???).last

  private val iter: Seq[(Int, Int)] = Seq.unfold((0, 2)) { case (from, to) =>
    val summed = input.slice(from, to).sum
    val compared = summed.compareTo(target)
    if (compared < 0)
      Some((from -> (to + 1), from -> (to + 1)))
    else if (compared == 0)
      None
    else if (to - from > 2)
      Some((from + 1 -> to, from + 1 -> to))
    else
      Some((from + 1 -> (to + 1), from + 1 -> (to + 1)))
  }

  println(iter.last)
  val theSlice = input.slice(iter.last._1, iter.last._2)
  println(theSlice.min + theSlice.max)
}
