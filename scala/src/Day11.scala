import scala.annotation.tailrec
import scala.reflect.io.File
import scala.util.{Failure, Success, Try}

object Day11 extends App {
  type Board = List[List[Char]]

  val input: Board = File("Inputs/day11.txt").lines().map(_.toCharArray.toList).toList

  def neighbours(x:Int, y:Int, board: Board): Seq[Char] = for (
    x$ <- (x-1) to (x+1);
    y$ <- (y-1) to (y+1);
    if Try(board(y$)(x$)).isSuccess;
    if (x$, y$) != (x, y)
  ) yield board(y$)(x$)

  def next(x:Int, y:Int, in: Board): Char = {
    in(y)(x) match {
      case '.' => '.'
      case 'L' => if (neighbours(x,y,in).count(_ == '#')==0) '#' else 'L'
      case '#' => if (neighbours(x,y,in).count(_ == '#')>3) 'L' else '#'
      case _ => ???
    }
  }

  def nextGen(b: Board): Board = b.indices.toList.map(y => b(y).indices.toList.map(x => next(x,y,b)))

  def gens = Iterator.unfold(input) { i => Some(i, nextGen(i)) }

  private val answer: Board = gens.sliding(2).dropWhile(p => p.head != p.last).next().head
  println(answer.mkString("\n"))
  println(answer.flatten.count(_ == '#'))

  @tailrec
  def view(x:Int, y:Int, dx:Int, dy:Int, board: Board): Char = Try(board(y)(x)) match {
    case Success('.') => view(x+dx, y+dy, dx, dy, board)
    case Success(c) => c
    case Failure(_) => '.'
  }

  def neighbours2(x:Int, y:Int, board: Board): Seq[Char] = for {
    dx <- -1 to 1;
    dy <- -1 to 1;
    if (dx, dy) != (0,0)
  } yield view(x + dx,y+dy,dx,dy, board)

  def next2(x:Int, y:Int, board: Board): Char = {
    board(y)(x) match {
      case '.' => '.'
      case 'L' =>
        val nbrs = neighbours2(x, y, board)
        if (nbrs.count(_ == '#') == 0) '#' else 'L'
      case '#' =>
        val nbrs = neighbours2(x, y, board)
        if (nbrs.count(_ == '#') > 4) 'L' else '#'
    }
  }

  def nextGen2(b: Board): Board = b.indices.toList.map(y => b(y).indices.toList.map(x => next2(x,y,b)))

  def gens2 = Iterator.unfold(input) { i => Some(i, nextGen2(i)) }

  val test = """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL""".split("\n").map(_.trim.toCharArray.toList).toList

//  println(test.map(_.mkString).mkString("\n"))
//  private val board2: Board = nextGen2(test)
//  println(board2.map(_.mkString).mkString("\n"))
//  println(nextGen2(board2).map(_.mkString).mkString("\n"))

  private val answer2: Board = gens2.sliding(2).dropWhile(p => p.head != p.last).next().head
  println(answer2.mkString("\n"))
  println(answer2.flatten.count(_ == '#'))

}
