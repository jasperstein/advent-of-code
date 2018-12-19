package day19

import java.lang.Integer.parseInt

object day19 extends App {

  import day16.day16OpCodes._

  val ipReg = 3

  val input: Map[Int, OpCode] = day19Input.input.lines.toList.tail.map(s => {
    val instr = s.split(" ")
    parseOpCode(instr(0))(parseInt(instr(1)), parseInt(instr(2)), parseInt(instr(3)))
  }).zipWithIndex.map(_.swap).toMap

  var regs = List(10551344,10551343,0,2,11*742,1291).zipWithIndex.map(_.swap).toMap
//  var regs = List(1,0,0,0,0,0).zipWithIndex.map(_.swap).toMap

  println(input.toList.sortBy(_._1))

  var i = 0
  var ip = 3

  while (input.get(ip).isDefined) {
    if (i<200) print(s"($i) $ip [${regs.toList.sortBy(_._1).map(_._2)}] ${input(ip)} ")
    regs = input(ip).exec(regs + (ipReg -> ip))
    if (i<200) println(regs.toList.sortBy(_._1).map(_._2))
    ip = regs(ipReg) + 1
    i = i + 1
  }

  //1 10551343 0 9 10551344 5
  //0 x 0 2 2 1
  //0 x 0 2 3 1

  //1 x 0 2 2 2
  //1 x 0 2 3 2

  // 11*743*1291
  println(i, regs.toList.sortBy(_._1).map(_._2))

}
