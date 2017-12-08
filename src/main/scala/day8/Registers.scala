package day8

object Registers extends App {

  case class Instruction(regToModify: String, delta: Int, regToCheck: String, operator: (Int, Int) => Boolean, compValue: Int)

  def parseLine(line:String): Instruction = {
    val tokens = line.split("\\s+").map(_.trim)
    val regToModify = tokens(0)
    val delta = Integer.parseInt(tokens(2)) * (if (tokens(1).equals("inc")) 1 else -1)
    val regToCheck = tokens(4)
    val compValue = Integer.parseInt(tokens(6))
    val operator: (Int, Int) => Boolean = tokens(5) match {
      case "<" => _ < _
      case "<=" => _ <= _
      case "==" => _ == _
      case "!=" => _ != _
      case ">=" => _ >= _
      case ">" => _ > _
    }
    Instruction(regToModify, delta, regToCheck, operator, compValue)
  }

  def process(input: String): Map[String, Int] = {
    input.lines.map(parseLine).foldLeft(Map[String, Int]())((registers, instr) => {
      val needToModify = instr.operator.apply(registers.getOrElse(instr.regToCheck, 0), instr.compValue)
      if (!needToModify) registers
      else {
        val newValue = registers.getOrElse(instr.regToModify, 0) + instr.delta
        registers + (instr.regToModify -> newValue)
      }
    })
  }

  println(process(Input.example))
  println(process(Input.star1))
  println(process(Input.star1).values.max)

}
