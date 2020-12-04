import scala.reflect.io.File

object Day3 extends App {
  private val input: List[String] = File("Inputs/day3.txt").lines().toList

  private def slope(n:Int): List[(Int, String)] = input.zipWithIndex.map({ case (line, idx) => (n * idx % line.length, line) })
  private def slope2: List[(Int, String)] = input.zipWithIndex.map({ case (line, idx) => (if (idx % 2 == 1) -1 else (idx / 2) % line.length, line) })

  private val count1: Int = slope(1).count({ case (i, l) => l.charAt(i) == '#' })
  println(count1)
  private val count3: Int = slope(3).count({ case (i, l) => l.charAt(i) == '#' })
  println(count3)
  private val count5: Int = slope(5).count({ case (i, l) => l.charAt(i) == '#' })
  println(count5)
  private val count7: Int = slope(7).count({ case (i, l) => l.charAt(i) == '#' })
  println(count7)
  private val count2: Int = slope2.count({ case (i, l) => i >= 0 && l.charAt(i) == '#' })
  println(count2)

  println(count1.toLong * count3.toLong * count5.toLong * count7.toLong * count2.toLong)

}
