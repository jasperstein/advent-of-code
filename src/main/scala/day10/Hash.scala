package day10

object HashApp extends App {
  import Hash._

  // example
  println(process(List(3, 4, 1, 5), 5))
  // for real
  println(process(List(212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164), 256))

  // examples
  println(hash(""))
  println(hash("AoC 2017"))
  println(hash("1,2,3"))
  println(hash("1,2,4"))
  // for real
  println(hash(inputRaw))
}

object Hash {
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

  // star2
  val inputRaw = "212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164"

  def round(inputs: List[Int], state: State): State = inputs.foldLeft(state)(transition)

  def rounds(n: Int, input: List[Int], state: State) = Range(0,n).foldLeft(state)((st, i) => round(input, st))

  def sparse(inputRaw: String): State = {
    val initialState = State(0, 0, Range(0, 256).toList)
    val input = inputRaw.map(_ + 0).toList ++ List(17, 31, 73, 47, 23)
    rounds(64, input, initialState)
  }

  def hashBlock(input: List[Int]): Int = input.foldLeft(0)((b1, b2) => b1 ^ b2)

  def dense(hash: List[Int]): List[Int] = hash.grouped(16).map(hashBlock).toList

  def hash(input: String): String = dense(sparse(input).hash).map(d => ("0" + d.toHexString).takeRight(2)).mkString("")

}
