package day19

import java.lang.Integer.parseInt

object day19 extends App {

  import day16.day16OpCodes._

  val ipReg = 3

  val input: Map[Int, OpCode] = day19Input.input.lines.toList.tail.map(s => {
    val instr = s.split(" ")
    parseOpCode(instr(0))(parseInt(instr(1)), parseInt(instr(2)), parseInt(instr(3)))
  }).zipWithIndex.map(_.swap).toMap

  var regs = List(0,0,0,0,0,0).zipWithIndex.map(_.swap).toMap

  println(input.toList.sortBy(_._1))

  var i = 0
  var ip = 0

  while (input.get(ip).isDefined) {
//    print(s"($i) $ip [${regs.toList.sortBy(_._1).map(_._2)}] ${input(ip)} ")
    regs = input(ip).exec(regs + (ipReg -> ip))
//    println(regs.toList.sortBy(_._1).map(_._2))
    ip = regs(ipReg) + 1
    i = i + 1
  }

  println(i, regs.toList.sortBy(_._1).map(_._2))

}
