import scala.reflect.io.File
import scala.util.Try

object Day14 extends App {
  private val input: List[String] = File("Inputs/day14.txt").lines().toList

  private val example: List[String] = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0""".split("\n").toList


  def mask(m: String): Array[Option[Int]] =
    m.toCharArray.map(c => Try(c.toString.toInt).toOption)

  def to36bits(l: Long): Array[Int] =
    (("0"*36)+l.toBinaryString).takeRight(36).toCharArray.map(_.toString.toInt)

  def setBits(i: Int, l: Long, m: String, mem: Map[Int, Array[Int]]): Map[Int, Array[Int]] =
    mem + (i -> mask(m).zip(to36bits(l)).map(p => p._1.getOrElse(p._2)))

  def bits2long(bits: Array[Int]): Long =
    bits.reverse.zipWithIndex.filter(_._1 == 1).map(i => math.pow(2,i._2).toLong).sum

  object IsMask {
    def unapply(s: String): Option[String] =
      if (s.substring(4).startsWith(" ")) Some(s.substring(7).trim) else None
  }

  object SetMem {
    def unapply(s: String): Option[(Int, Long)] =
      if (s.substring(4).startsWith(" ")) None
      else Some(s.substring(4).split(Array(']', ' ', '='))).map(parts => (parts.head.toInt, parts.last.toLong))
  }

  def readDockingParameters(l: List[String]): Map[Int, Array[Int]] =
    l.foldLeft(("", Map[Int, Array[Int]]()))({ case ((currMask, mem), line) =>
      line match {
        case IsMask(newMask) => (newMask, mem)
        case SetMem((k, v)) => (currMask, setBits(k, v, currMask, mem))
      }
    })._2

  println(readDockingParameters(example).values.map(bits2long).sum)
  println(readDockingParameters(input).values.map(bits2long).sum)

  println(input.filter(_.startsWith("mask")).map(_.count(_ == 'X')).sorted)
}
