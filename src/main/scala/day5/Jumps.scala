package day5

object Jumps extends App {

  def stringToInts(input: String): Seq[Int] = input.split("\\s+").map(c => Integer.parseInt(c.toString))

  implicit class MemState(memState: Seq[Int]) {
    def incrementAt(i:Int): Seq[Int] = memState.updated(i, memState(i) + 1)
  }

  case class State(stepsTaken: Int, position: Int, memState: Seq[Int]) {
    def next(): State = State(stepsTaken + 1, position + memState(position), memState.incrementAt(position))
  }

  def jumps(mem: Seq[Int]): Unit = {
    var state = State(0, 0, mem)
    while (mem.isDefinedAt(state.position)) {
      println(s"Step ${state.stepsTaken}, position ${state.position}")
      state = state.next()
    }
    println(s"DONE! Step ${state.stepsTaken}, position ${state.position}")
  }

  // example
  jumps(stringToInts("0 3 0 1 -3"))

  // for real
  jumps(stringToInts(Input.star1))
}
