import scala.io.Source
object Day2 extends App {
    val input = Source.fromFile("inputs/day2.txt").getLines().map(l => l.charAt(0) -> l.charAt(2)).toSeq
  val inputNumerical = input.map { case (c1, c2) => (c1.toInt - 'A'.toInt + 1) -> (c2.toInt - 'X'.toInt + 1)}
  def roundScore(turns: (Int, Int)): Int = (((turns._1 - turns._2 + 3) % 3) match {
    case 0 => 3
    case 1 => 0
    case 2 => 6
  }) + turns._2
  
def realRoundScore(turns: (Int, Int)): Int = (turns._2 - 1) * 3 + (turns._2 match {
    case 1 => if (turns._1 == 1) 3 else (turns._1 - 1)
    case 2 => turns._1
    case 3 => if (turns._1 == 3) 1 else (turns._1 + 1)
})

  println(inputNumerical.map(roundScore).sum)
  println(inputNumerical.map(realRoundScore).sum)


}
