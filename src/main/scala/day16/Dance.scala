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
  println(parseInput(Input.star1).foldLeft(initLineup){ case ((lineup, move)) => move.dance(lineup) }.mkString(""))
}
