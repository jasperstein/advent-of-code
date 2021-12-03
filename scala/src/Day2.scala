import scala.io.Source
object Day2 extends App {
  val input = Source.fromFile("Inputs/day2.txt").getLines.toList

  def read(direction: String)(line: Array[String]) = line(1).toIntOption.filter(_ => line.head == direction && line.length == 2)

  object Forward { def unapply(line: Array[String]): Option[Int] = read("forward")(line) }
  object Down { def unapply(line: Array[String]): Option[Int] = read("down")(line) }
  object Up { def unapply(line: Array[String]): Option[Int] = read("up")(line)}

  def move1(curr: (Int, Int), instr: String): (Int, Int) = instr.split("\\s+").take(2) match {
      case Forward(n) => (curr._1 + n, curr._2)
      case Down(n) => (curr._1, curr._2 + n)
      case Up(n) => (curr._1, curr._2 - n)
      case a => { println(s"X${a.head}Y${a(1)}Z"); curr }
  }
  val answer1: (Int, Int) = input.foldLeft((0,0))(move1)
  
  def move2(curr: (Int, Int, Int), instr: String): (Int, Int, Int) = instr.split("\\s+").take(2) match {
      case Forward(n) => (curr._1 + n, curr._2 + n*curr._3, curr._3)
      case Down(n) => (curr._1, curr._2, curr._3 + n)
      case Up(n) => (curr._1, curr._2, curr._3 - n)
  }
  val answer2: (Int, Int, Int) = input.foldLeft((0,0,0))(move2)
  
  println(answer1._1 * answer1._2)
  println(answer2._1 * answer2._2)
}
