import scala.reflect.io.File

object Day10 extends App {
  private val input: List[Int] = File("Inputs/day10.txt").lines().map(Integer.parseInt).toList

  def sorted(input: List[Int]): List[Int] = input.sorted :+ (input.max + 3)

  private def result(sorted: List[Int]): (Int, Int, Int) = sorted.foldLeft((0, 0, 0)) {
    case ((ones, threes, last), current) =>
      val diff = current - last
      print(diff + " ")
      (ones + (if (diff == 1) 1 else 0),
        threes + (if (diff == 3) 1 else 0),
        current)
  }

  println(result(sorted(input))._1 * result(sorted(input))._2)

  val smallExample = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)

  val largerExample = List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)

  case class State(stack: List[Int], pathCounts: Map[Int, Long])

  def initialState(in: List[Int]): State = {
    val desc = sorted(in).+:(0).reverse
    println(desc)
    State(desc.tail, Map(desc.head -> 1))
  }

  def nextState(curr: State): State = {
    val item = curr.stack.head
    val tooWideGap = curr.pathCounts.keySet.filter(k => k - item > 3)
    val possiblePaths = curr.pathCounts -- tooWideGap
    val newPathCounts = possiblePaths + (item -> possiblePaths.values.sum)
    State(curr.stack.tail, newPathCounts)
  }

  def seq(in: List[Int]): Seq[State] = Seq.unfold(initialState(in)) { st =>
    println(st)
    if (st.stack.isEmpty) None else Some(nextState(st), nextState(st))
  }

  println(seq(smallExample).last)
  println(seq(largerExample).last)
  println(seq(input).last)

}
