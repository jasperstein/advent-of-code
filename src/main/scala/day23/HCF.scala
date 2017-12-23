package day23

object HCF extends App {

  def parseInput(input: String): List[Instruction] = {
    input.lines.toList.map(l => l.split(" ").toList match {
      case "set" :: r :: to :: Nil => SetReg(r.charAt(0), to)
      case "sub" :: r :: to :: Nil => Sub(r.charAt(0), to)
      case "mul" :: r :: to :: Nil => Mul(r.charAt(0), to)
      case "jnz" :: r :: to :: Nil => Jnz(r, to)
    })
  }

  private val instructions: List[Instruction] = parseInput(Input.star1)
  println(instructions.zipWithIndex.mkString("\n"))

  val initRegs = Map('a' -> 0L, 'b' -> 0L, 'c' -> 0L, 'd' -> 0L, 'e' -> 0L, 'f' -> 0L, 'g' -> 0L, 'h' -> 0L)
  var state = State(0, initRegs, 0)
  while (instructions.indices.contains(state.pc)) {
    val instruction = instructions(state.pc)
    state = instruction.apply(state)
    println(instruction.toString + state)
  }
}
