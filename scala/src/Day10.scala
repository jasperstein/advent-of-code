import scala.io.Source
object Day10 extends App {
  val input = Source.fromFile("Inputs/day10.txt").getLines.toList.map(_.toCharArray.toList).toList

  val pairs: Map[Char, Char] = Map ('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  trait State { 
    val stack: List[Char]
  }
  case class OK(stack: List[Char]) extends State
  case class NOK(stack: List[Char], unexpected: Char) extends State

  def check(line: List[Char]) = line.foldLeft[State](OK(List())) { case (st, c) =>
    st match {
        case NOK(_, _) => st
        case OK(stack) =>
            if (pairs.keySet.contains(c)) OK(c :: stack) 
            else if (pairs(stack.head) == c) OK(stack.tail)
            else NOK(stack, c)
    }
  }

  val points = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  println(input.map(check).collect { case NOK(_, c) => c }.map(points).sum)

  val points2 = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)

  val scores = 
      input.map(check).collect { case ok@OK(st) => st}
      .map{_.foldLeft(0L){
       case (score, ch) => 5*score + points2(ch)
    }}
  println(scores.sorted.apply(scores.length/2))
}