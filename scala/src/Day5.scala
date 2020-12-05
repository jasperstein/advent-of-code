import scala.reflect.io.File

object Day5 extends App {
  private val input: List[String] = File("Inputs/day5.txt").lines().toList

  def op(c: Char): Seq[Int] => Seq[Int] = c match {
    case 'F' | 'L' => r => r.take(r.length/2)
    case 'B' | 'R' => r => r.takeRight(r.length/2)
  }

  def seatId: String => Int = line => {
    val row = line.take(7).foldLeft(0 to 127: Seq[Int])({case (r, c) => op(c)(r)})
    val col = line.slice(7, 10).foldLeft(0 to 7: Seq[Int])({case (r, c) => op(c)(r)})
    row.head * 8 + col.head
  }

  println(input.maxBy(seatId))
  println(seatId(input.maxBy(seatId)))

  val from = seatId(input.minBy(seatId))
  val to = seatId(input.maxBy(seatId))
  println((from to to).diff(input.map(seatId(_))))
}
