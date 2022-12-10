import scala.io.Source
object Day4 extends App {
    val input = Source.fromFile("inputs/day4.txt").getLines().toSeq
  def ranges(line: String) = line.trim().split("-").flatMap(_.split(",")).toList.take(4).map(_.toInt)
  def contained(ranges: List[Int]): Boolean = 
    (ranges.sorted == List(ranges(0), ranges(2), ranges(3), ranges(1))) ||
    (ranges.sorted == List(ranges(2), ranges(0), ranges(1), ranges(3)))

    println(input.map(ranges).count(contained))

    def overlapping(ranges: List[Int]): Boolean = contained(ranges) || (ranges match {
        case a1 :: a2 :: b1 :: b2 :: Nil =>
            (a1 to a2).contains(b1) || (a1 to a2).contains(b2) || (b1 to b2).contains(a1) || (b1 to b2).contains(a2)
    })
  println(overlapping(ranges("2-4,6-8")))
  println(overlapping(ranges("2-3,4-5")))
  println(overlapping(ranges("5-7,7-9")))
  println(overlapping(ranges("2-8,3-7")))
  println(overlapping(ranges("6-6,4-6")))
  println(overlapping(ranges("2-6,4-8")))
  println(input.map(ranges).count(overlapping))

}
