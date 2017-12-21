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

  def step(p: Particle): Particle = {
    val newV = Vect(p.v.x + p.a.x, p.v.y + p.a.y, p.v.z + p.a.z)
    val newP = Vect(p.p.x + newV.x, p.p.y + newV.y, p.p.z + newV.z)
    Particle(newP, newV, p.a)
  }

  def collide(ps: List[Particle]): List[Particle] = {
    val coords = ps.map(_.p)
    val collisions = coords.filter(p => coords.count(_ == p) > 1)
    if (collisions.nonEmpty) println(collisions)
    ps.filterNot(p => collisions.contains(p.p))
  }

  var particles = parseInput(Input.star1)
  for (i <- Range(0, 20000)) { // 20000 should be enough for anybody
    println(i)
    particles = particles.map(step)
    particles = collide(particles)
  }
  println(particles.size)

}
