import scala.io.Source
object Day5 extends App {
    val input = Source.fromFile("inputs/day5.txt").getLines().toSeq
    val stackSpec = input.takeWhile(_.endsWith("]")).map(_.toList.zipWithIndex.filter(_._2 % 4 == 1).map(_._1))
    val stacks = (1 to 9).map(i => i -> stackSpec.map(_(i - 1)).filterNot(_ == ' ')).toMap
    val moves = input.filter(_.startsWith("move")).map(_.split(" ")).map(str => (str(1).toInt, str(3).toInt, str(5).toInt))

    def move1(stack: Map[Int, Seq[Char]], move: (Int, Int)): Map[Int, Seq[Char]] = move match {
        case (from, to) => stack(from) match {
            case c :: cs => stack + (from -> cs) + (to -> (c +: stack(to)))
        }
    }

    def allMoves(moves: Seq[(Int, Int, Int)]) = moves.flatMap { case (n, from, to) => Seq.fill(n)(from -> to) }

    val movedStacks = allMoves(moves).foldLeft(stacks)(move1).toList.sortBy(_._1).map(_._2.head)
    println(movedStacks.mkString)

    def moveStack(stack: Map[Int, Seq[Char]], move: (Int, Int, Int)): Map[Int, Seq[Char]] = move match {
        case (n, from, to) => stack(from).splitAt(n) match {
            case (top, bottom) => stack + (from -> bottom) + (to -> (top ++ stack(to)))
        }
    }

    val moved9001 = moves.foldLeft(stacks)(moveStack).toList.sortBy(_._1).map(_._2.head)
    println(moved9001.mkString)
}
