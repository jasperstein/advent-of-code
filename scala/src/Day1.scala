import scala.annotation.tailrec
import scala.reflect.io.File

object Day1 extends App {
  private val input: List[Int] = File("Inputs/day1.txt").lines().map(Integer.parseInt).toList

  @tailrec
  def find(target: Int, l: List[Int]): Either[Unit, (Int, Int)] =
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

  find(2020, input.sorted) match {
    case Right((a,b)) => println(s"$a * $b = ${a*b}")
    case Left(_) => println("Not Found")
  }

  @tailrec
  def find2(l: List[Int]): Unit = l match {
    case Nil => println("Not found")
    case ::(head, next) => find(2020 - head, next) match {
      case Right((a,b)) =>
        println(s"$head * $a * $b = ${head * a * b}")
      case Left(_) =>
        find2(next)
    }
  }

  find2(input.sorted)
}
