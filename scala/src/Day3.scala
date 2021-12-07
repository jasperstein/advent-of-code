import scala.io.Source
object Day3 extends App {
  val input = Source.fromFile("Inputs/day3.txt").getLines.toList.map(_.trim.split(""))
  
  val initial = Array.fill(input.head.length)(0)
  def count(curr: Array[Int], line: Array[String]): Array[Int] = curr.zip(line.map(_.toInt)).map { case (count, bit) => count + bit }

  def oneCounts(input: List[Array[String]]) = input.foldLeft(initial)(count)
  val gammaString = oneCounts(input).map(n => if (n > input.length / 2) "1" else "0").mkString("")
  val epsilonString = oneCounts(input).map(n => if (n > input.length / 2) "0" else "1").mkString("")

  val gamma = Integer.parseInt(gammaString, 2) 
  val epsilon = Integer.parseInt(epsilonString, 2)

  println(s"$gammaString ($gamma) * $epsilonString ($epsilon) = ${gamma * epsilon}")

  def finder(i: Int, input: List[Array[String]], bitCriterion: Int => Int => String => Boolean): List[Array[String]] = {
      println(s"i = $i, # = ${input.length}")
      val counts = oneCounts(input)
      println(s"1counts: ${counts.toList}")
      input.filter(line => bitCriterion(counts(i))(input.length)(line(i)))
  }

  val o2gen: Int => Int => String => Boolean =
      ones => total => ch => if (ones >= total / 2.0) ch == "1" else ch == "0"
  val co2scr: Int => Int => String => Boolean =
      ones => total => ch => if (ones >= total / 2.0) ch == "0" else ch == "1"

  def find(i: Int, input: List[Array[String]], bitCriterion: Int => Int => String => Boolean): Array[String] = {
      val found = finder(i, input, bitCriterion)
      if (found.length > 1) find(i+1, found, bitCriterion) else found.head
  }

  val o2rating = Integer.parseInt(find(0, input, o2gen).mkString(""), 2)
  val co2rating = Integer.parseInt(find(0, input, co2scr).mkString(""), 2)
  println(s"o2: $o2rating")
  println(s"co2: $co2rating")
  println(o2rating * co2rating)



}
