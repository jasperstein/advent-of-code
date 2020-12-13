import scala.annotation.tailrec
import scala.reflect.io.File
import scala.util.Try

object Day13 extends App {
  private val input: List[String] = File("Inputs/day13.txt").lines().toList

  def buses(input: String) = input.split(",").toList
  def active(input: String) = buses(input).filter(b => Try(b.toInt).isSuccess).map(_.toInt)
  val waitingTimes = active(input.last).map(b => (b, b - (input.head.toInt % b)))

  private val myBus: (Int, Int) = waitingTimes.minBy(_._2)
  println(myBus)
  println(myBus._1 * myBus._2)

  def isPrime(i:Int) = (2 to i/2).forall(i % _ > 0)
  println(active(input.last).forall(isPrime))

  def activeBusPositions(input: String) = buses(input).zipWithIndex.collect({
    case (b, pos) if Try(b.toInt).isSuccess => (b.toLong, pos.toLong)
  })

  println(activeBusPositions(input.last))

  case class Stepper(candidate: Long, stepSize: Long)

  @tailrec
  def findFirst(stepper: Stepper, busPosition: (Long, Long)): Stepper = {
    println(s"$stepper $busPosition")
    (stepper, busPosition) match {
      case (Stepper(candidate, stepSize), (bus, position)) =>
        if (candidate % bus == bus - (position % bus)) stepper.copy(stepSize = stepSize * bus)
        else findFirst(stepper.copy(candidate = candidate + stepSize), busPosition)
    }
  }

  private def stepper(input: String): Stepper = activeBusPositions(input).tail.foldLeft(Stepper(0L, activeBusPositions(input).head._1))({ case (stepper, busPosition) => findFirst(stepper, busPosition) })

//  The earliest timestamp that matches the list 17,x,13,19 is 3417.
//67,7,59,61 first occurs at timestamp 754018.
//67,x,7,59,61 first occurs at timestamp 779210.
//67,7,x,59,61 first occurs at timestamp 1261476.
//1789,37,47,1889 first occurs at timestamp 1202161486.

  println(stepper("17,x,13,19"))
  println(stepper("67,7,59,61"))
  println(stepper("67,x,7,59,61"))
  println(stepper("67,7,x,59,61"))
  println(stepper("1789,37,47,1889"))

  private val inputStepper: Stepper = stepper(input.last)
  println(inputStepper.candidate)
}
