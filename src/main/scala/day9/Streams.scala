package day9

object Streams extends App {

  case class ParseState(currentScore: Int, garbageCount: Int, depth: Int, garbage: Boolean, ignoreNext: Boolean)

  def transition(state: ParseState, ch: Char): ParseState = {
    if (state.ignoreNext) state.copy(ignoreNext = false)
    else if (state.garbage) {
      ch match {
        case '>' => state.copy(garbage = false)
        case '!' => state.copy(ignoreNext = true)
        case _ => state.copy(garbageCount = state.garbageCount + 1)
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

  val beginState = ParseState(0, 0, 0, garbage = false, ignoreNext = false)

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

  def garbage(input: String): Int = input.foldLeft(beginState)(transition).garbageCount
  println(garbage(Input.garbage1))
  println(garbage(Input.garbage2))
  println(garbage(Input.garbage3))
  println(garbage(Input.garbage4))
  println(garbage(Input.garbage5))
  println(garbage(Input.garbage6))
  println(garbage(Input.garbage7))
  println(garbage(Input.star1))

}
