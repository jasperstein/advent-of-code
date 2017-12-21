package day20

object Particles extends App {

  case class Vect(x: Int, y: Int, z:Int) {
    def manhattan: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)
  }
  case class Particle(p: Vect, v: Vect, a: Vect)

  val regex = "p=\\<(-?\\d+),(-?\\d+),(-?\\d+)\\>, v=\\<(-?\\d+),(-?\\d+),(-?\\d+)\\>, a=\\<(-?\\d+),(-?\\d+),(-?\\d+)\\>".r

  def parseInput(input: String): List[Particle] = input.lines.toList
    .map(regex.unapplySeq(_).get.map(Integer.parseInt))
    .map(cs => Particle(Vect(cs(0), cs(1), cs(2)), Vect(cs(3), cs(4), cs(5)), Vect(cs(6), cs(7), cs(8))))

  println (parseInput(Input.star1).zipWithIndex.minBy(_._1.a.manhattan))
  println (parseInput(Input.star1).zipWithIndex.filter(_._1.a.manhattan == 2))

}
