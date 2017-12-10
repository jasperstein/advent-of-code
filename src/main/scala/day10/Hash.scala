package day10

object Hash extends App {
  case class State(skipSize: Int, currPos: Int, hash: List[Int])

  def transition(state: State, input: Int): State = {
    val result = Array[Int](state.hash: _*)
    val doubled = state.hash ++ state.hash
    val toReverse = doubled.slice(state.currPos, state.currPos + input)
    val reversed = toReverse.reverse
    for (i <- toReverse.indices) {
      result((state.currPos + i) % state.hash.length) = reversed(i)
    }
    State(state.skipSize + 1, (state.currPos + input + state.skipSize) % state.hash.length, result.toList)
  }

  def process(inputs: List[Int], arraySize: Int): Int = {
    val initial = Range(0, arraySize).toList
    val result: State = inputs.foldLeft(State(0, 0, initial))(transition)
    result.hash.head * result.hash(1)
  }

  // example
  println(process(List(3, 4, 1, 5), 5))
  // for real
  println(process(List(212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164), 256))
}
