import scala.io.Source
object Day1 extends App {
    val input = Source.fromFile("Inputs/day1.txt").getLines.toList.map(_.toInt)

    def countIncreases(input: List[Int]): Int = input.zip(input.tail).count { case (a,b) => a < b}
   
    def slidingWindowSum(input: List[Int]): List[Int] = input.zip(input.tail).zip(input.drop(2)).map{ case ((a,b),c) => a+b+c}
   
    println(countIncreases(input))
    println(countIncreases(slidingWindowSum(input)))
}