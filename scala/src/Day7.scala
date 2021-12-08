import scala.io.Source
object Day7 extends App {
  val input = Source.fromFile("Inputs/day7.txt").getLines.next.split(",").map(Integer.parseInt)
  val fuels = (input.min to input.max).map(pos => pos -> input.map(i => Math.abs(pos - i)).sum)
  println(fuels.minBy(_._2))
  val fuels2 = 
      (input.min to input.max).map(pos => 
          pos -> input.map(i => Math.abs(pos - i)*(Math.abs(pos - i) + 1)/2).sum)
  println(fuels2.minBy(_._2))
}
