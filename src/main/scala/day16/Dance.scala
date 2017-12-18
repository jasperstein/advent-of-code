package day16

object Dance extends App {

  def parseInput(input:String): List[Move] = input.split(",").map(item => item.charAt(0) match {
    case 's' => Spin(Integer.parseInt(item.tail))
    case 'x' =>
      val args = item.tail.split("/").map(Integer.parseInt)
      Exchange(args(0), args(1))
    case 'p' => Partner(item.charAt(1), item.charAt(3))
    case _ => throw new Exception
  }).toList


  println(parseInput("s1,x3/4,pe/b").foldLeft(List('a','b','c','d','e')){ case ((lineup, move)) => move.dance(lineup) }.mkString(""))

  val initLineup = Range(0, 16).map(i => ('a' + i).asInstanceOf[Char]).toList
  private val moves: List[Move] = parseInput(Input.star1)

  private def dance(lineup: List[Char]) = {
    moves.foldLeft(lineup) { case ((currentlineup, move)) => move.dance(currentlineup) }
  }

  println(dance(initLineup).mkString(""))

  Range(0, 100).foldLeft(initLineup)({case ((lineup, i)) => println(lineup.mkString("") + i); dance(lineup)})
  println(1000000000 % 42)
}
