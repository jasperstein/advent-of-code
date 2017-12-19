package day18

object Duet extends App {
  def parseInput(input: String): List[Instruction] = {
    input.lines.toList.map(l => l.split(" ").toList match {
      case "set" :: r :: to :: Nil => SetReg(r.charAt(0), to)
      case "add" :: r :: to :: Nil => Add(r.charAt(0), to)
      case "mul" :: r :: to :: Nil => Mul(r.charAt(0), to)
      case "mod" :: r :: to :: Nil => Mod(r.charAt(0), to)
      case "snd" :: f :: Nil => Snd(f)
      case "rcv" :: r :: Nil => Rcv(r.charAt(0))
      case "jgz" :: r :: to :: Nil => Jgz(r, to)
    })
  }

  private val exampleInstructions: List[Instruction] = parseInput(Input.example)
  println(exampleInstructions)

  private val instructions: List[Instruction] = parseInput(Input.star1)
  println(instructions)

  val initRegs = Map('a' -> 0L, 'b' -> 0L, 'j' -> 0L, 'p' -> 0L, 'i' -> 0L, 'f' -> 0L)
  var state = State(0, initRegs, 0)
  while (instructions.indices.contains(state.pc)) {
    state = instructions(state.pc).apply(state)
    println(state)
  }
}
