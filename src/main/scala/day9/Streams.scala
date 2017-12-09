package day9

object Streams extends App {

  case class ParseState(currentScore: Int, depth: Int, garbage: Boolean, ignoreNext: Boolean)

  def transition(state: ParseState, ch: Char): ParseState = {
    if (state.ignoreNext) state.copy(ignoreNext = false)
    else if (state.garbage) {
      ch match {
        case '>' => state.copy(garbage = false)
        case '!' => state.copy(ignoreNext = true)
        case _ => state
      }
    } else {
      ch match {
        case '{' => state.copy(depth = state.depth + 1)
        case '}' => state.copy(depth = state.depth - 1, currentScore = state.currentScore + state.depth)
        case '<' => state.copy(garbage = true)
        case _ => state
      }
    }
  }

  val beginState = ParseState(0, 0, garbage = false, ignoreNext = false)

  def score(input: String): Int = input.foldLeft(beginState)(transition).currentScore

  println(score(Input.example1))
  println(score(Input.example2))
  println(score(Input.example3))
  println(score(Input.example4))
  println(score(Input.example5))
  println(score(Input.example6))
  println(score(Input.example7))
  println(score(Input.example8))
  println(score(Input.star1))

}
