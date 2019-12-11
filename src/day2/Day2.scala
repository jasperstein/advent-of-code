package day2

import scala.reflect.io.{File, Path}

object Day2 extends App {
  type Mem = Array[Int]
  private val origmem: Mem = File(Path("src/day2/Input.txt")).safeSlurp().get.split(",").map(Integer.parseInt)

  val opcodes: Map[Int, (Int, (Mem,Int) => Mem)] = Map(1 -> (4, addCmd), 2 -> (4, multCmd))

  val part1 = origmem.clone()
  part1.update(1,12)
  part1.update(2,2)
  println(List(runProg(part1, opcodes): _*))

  for (i <- 0 until 10000) {
    val mem = origmem.clone()
    mem.update(1, i / 100)
    mem.update(2, i % 100)
    val result = runProg(mem, opcodes)
    if (result(0) == 19690720) println(List(result: _*))
  }


  def addCmd(start: Mem, prgCounter: Int): Mem = {
    val mem = start.clone()
    val loc1 = mem(prgCounter + 1)
    val op1 = mem(loc1)
    val loc2 = mem(prgCounter + 2)
    val op2 = mem(loc2)
    val dest = mem(prgCounter + 3)
    mem.update(dest, op1 + op2)
    mem
  }

  def multCmd(start: Mem, prgCounter: Int): Mem = {
    val mem = start.clone()
    val loc1 = mem(prgCounter + 1)
    val op1 = mem(loc1)
    val loc2 = mem(prgCounter + 2)
    val op2 = mem(loc2)
    val dest = mem(prgCounter + 3)
    mem.update(dest, op1 * op2)
    mem
  }

  def runProg(start: Mem, opcodes: Map[Int, (Int, (Mem, Int) => Mem)]): Mem = {
    var mem = start.clone()
    var prgCounter = 0
    var nextCmd = mem(0)
    while (nextCmd != 99) {
      val (skip, op) = opcodes(nextCmd)
      mem = op(mem, prgCounter)
      prgCounter = prgCounter + skip
      nextCmd = mem(prgCounter)
      println(mem)
    }
    mem
  }
}
