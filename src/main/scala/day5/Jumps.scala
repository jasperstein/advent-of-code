package day5

object Jumps extends App {

  def stringToInts(input: String): Seq[Int] = input.split("\\s+").map(c => Integer.parseInt(c))

  implicit class MemState(memState: Seq[Int]) {
    def incrementAt(i: Int): Seq[Int] = memState.updated(i, memState(i) + 1)

    def newIncrement(i: Int): Seq[Int] = {
      val mem_i = memState(i)
      memState.updated(i, if (mem_i >= 3) mem_i - 1 else mem_i + 1)
    }
  }

  case class State(stepsTaken: Int, position: Int, memState: Seq[Int]) {
    def next(): State = State(stepsTaken + 1, position + memState(position), memState.incrementAt(position))

    def newNext(): State = State(stepsTaken + 1, position + memState(position), memState.newIncrement(position))
  }

  def jumps(mem: Seq[Int], newRules: Boolean = false): Unit = {
    var state = State(0, 0, mem)
    while (mem.isDefinedAt(state.position)) {
      // println(s"Step ${state.stepsTaken}, position ${state.position}")
      state = if (newRules) state.newNext() else state.next()
    }
    println(s"DONE! Step ${state.stepsTaken}, position ${state.position}")
  }

  // example
  jumps(stringToInts("0 3 0 1 -3"))

  // for real
  jumps(stringToInts(Input.star1))

  // example 2
  jumps(stringToInts("0 3 0 1 -3"), newRules = true)

  // part 2
  jumps(stringToInts(Input.star1), newRules = true)
}
