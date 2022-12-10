import scala.io.Source
import scala.util.Try
object Day1 extends App {
    val input = Source.fromFile("inputs/day1.txt").getLines().map(l => Try(Integer.parseInt(l)).toOption).toSeq
    val start = (0, 0)
    val result = input.foldLeft(start) {case ((cmax, curr), line) => line match {
        case None => (cmax, 0)
        case Some(i) => {
            val next = curr + i
            (Math.max(next, cmax), next)
        }
    }}
    println(result._1)

    val start2 = (Map[Int, Int]().withDefaultValue(0), 1)
    val calories = input.foldLeft(start2) { case ((cals, currElf), line) => line match {
        case None => (cals, currElf + 1)
        case Some(value) => (cals + (currElf -> (cals(currElf) + value)), currElf)
    }}._1

    val top3 = calories.values.toSeq.sorted.reverse.take(3).sum
    println(top3)
}