import scala.io.Source
object Day9 extends App {
  val input = Source.fromFile("Inputs/day9.txt").getLines.toList
  val heights = input.zipWithIndex.flatMap { case (hs, y) => hs.zipWithIndex.map { case (ch, x) => ((x,y) -> Integer.parseInt(ch.toString)) } }.toMap
  val (xMax, yMax) = (input.head.length, input.length)

  def neighbours(xy: (Int, Int)): Seq[(Int,Int)] = 
      Seq() ++ Some((xy._1 - 1, xy._2)).filter(_ => xy._1 > 0) ++ Some((xy._1 + 1, xy._2)).filter(_ => xy._1 < xMax - 1) ++
        Some((xy._1, xy._2 - 1)).filter(_ => xy._2 > 0) ++ Some((xy._1, xy._2 + 1)).filter(_ => xy._2 < yMax - 1)

  val risks = for {
      x <- 0 until xMax
      y <- 0 until yMax
      if neighbours((x,y)).forall(n => heights(n) > heights((x,y)))
  } yield (x,y) -> (heights((x,y))+1)

  println(risks.map(_._2).sum)

  type Pos= (Int, Int)
  case class State(seen: Set[Pos], basinSizes: Seq[Int])

  def growBasin(basin: Set[Pos]): Set[Pos] = basin.flatMap(pos => neighbours(pos).filter(n => heights(n) < 9) :+ pos)
  def growBasinAll(basin: Set[Pos]): Set[Pos] = {
      val next = growBasin(basin)
      if (next.size == basin.size) basin else growBasinAll(next)
  }
  def growBasinFrom(p: Pos) = growBasinAll(Set(p))


  val initial = State(heights.keySet.filter(p => heights(p) == 9), basinSizes = Seq())
  val finalState = heights.keySet.foldLeft(initial) { case (st, p) => if (st.seen.contains(p)) st else { val b = growBasinFrom(p); State(st.seen ++ b, st.basinSizes :+ b.size)}}

  val sizes = finalState.basinSizes.sorted.reverse

  println(sizes.head * sizes(1) * sizes(2))
}
