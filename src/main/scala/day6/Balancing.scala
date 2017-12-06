package day6

import scala.collection.mutable

object Balancing extends App {
  def stringToInts(input: String): Array[Int] = input.split("\\s+").map(c => Integer.parseInt(c))

  implicit class MemStepper(mem: Array[Int]) {
    def step(): Unit = {
      val max = mem.max
      val maxidx = mem.indexOf(max)
      mem(maxidx) = 0
      for (i <- 1 to max) {
        mem((maxidx + i) % mem.length) += 1
      }
    }
  }

  def countSteps(mem: Array[Int]): Unit = {
    var seen = List[Array[Int]]()
    while (!seen.exists(_.sameElements(mem))) {
      seen :+= Array[Int](mem: _*)
      mem.step()
    }
    println(seen.size)
    println(seen.size - seen.indexWhere(_.sameElements(mem)))
  }

  val example = "0 2 7 0"

  countSteps(stringToInts(example))

  val star1 = """10	3	15	10	5	15	5	15  9	2	5	8	5	2	3	6"""

  countSteps(stringToInts(star1))
}
