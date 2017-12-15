package day15

object Generators extends App {

  var found = 0
  var a = 722
  var b = 354

  def matches(genA:Long, genB:Long, factorA: Int = 16807, factorB: Int = 48271, rounds: Int = 40000000): Int = {
    var a = genA
    var b = genB
    var found = 0
    for (i <- Range(0, rounds)) {
      a = (a * factorA) % 2147483647
      b = (b * factorB) % 2147483647
      val aStr = a.toBinaryString.takeRight(16)
      val bStr = b.toBinaryString.takeRight(16)
//      println(s"$a\t$b")
//      println(aStr)
//      println(bStr)
//      println()
      if (aStr.equals(bStr)) found += 1
    }
    found
  }

  // example
  println(matches(65, 8921, rounds = 5))
  // for real
  println(matches(722, 354))
}
