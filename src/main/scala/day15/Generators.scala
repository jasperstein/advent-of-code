package day15

object Generators extends App {

  var found = 0
  var a = 722
  var b = 354

  // see previous commit for star 1
  def matches(genA:Long, genB:Long, factorA: Int = 16807, factorB: Int = 48271, rounds: Int = 5000000): Int = {
    var a = genA
    var b = genB
    var found = 0
    def step(gen: Long, factor: Int, mod: Int):Long = {
      val result = (gen * factor) % 2147483647
      if (result % mod == 0) result else step(result, factor, mod)
    }
    for (i <- Range(0, rounds)) {
      a = step(a, factorA, 4)
      b = step(b, factorB, 8)
      val aStr = a.toBinaryString.takeRight(16)
      val bStr = b.toBinaryString.takeRight(16)
      if (aStr.equals(bStr)) found += 1
    }
    found
  }

  // example
  println(matches(65, 8921, rounds = 1060))
  // for real
  println(matches(722, 354))
}
