package day4

import java.text.SimpleDateFormat
import java.util
import java.util.Date
import java.util.function.Consumer

import scala.collection.mutable

object day4 extends App {

  val input = day4Input.input.lines.toList

  val fmt = new SimpleDateFormat("yyyy-MM-dd hh:mm")
  println(input(0).substring(19).dropWhile(_ != '#').takeWhile(_ != ' ').tail)
  println(fmt.parse(input(1).substring(1, 17)))

  sealed trait GuardState { val when: Date }
  case class FallsAsleep(when: Date) extends GuardState
  case class WakesUp(when: Date) extends GuardState
  case class GuardDuty(when: Date, id: Int) extends GuardState

  def parseLine(line: String): GuardState = {
    val when = fmt.parse(line.substring(1,17))
    val rest = line.substring(19)
    if (rest.startsWith("falls asleep")) FallsAsleep(when)
    else if (rest.startsWith("wakes up")) WakesUp(when)
    else if (rest.startsWith("Guard")) GuardDuty(when, Integer.parseInt(rest.dropWhile(_ != '#').takeWhile(_ != ' ').tail))
    else throw new NumberFormatException
  }

  val states: List[GuardState] = input.map(parseLine).sortBy(_.when)

  println(states)
  var sleepMinutes = mutable.Map[Int, (Int, util.ArrayList[(Int, Int)])]().withDefault(_ => (0, new util.ArrayList[(Int,Int)]()))
  var currentGuard = 0
  var currStartSleep = 0
  for (state <- states) {
    state match {
      case GuardDuty(when, id) => currentGuard = id
      case FallsAsleep(when) => currStartSleep = when.getMinutes
      case WakesUp(when) =>
        sleepMinutes(currentGuard)._2.add((currStartSleep, when.getMinutes))
        sleepMinutes(currentGuard) = (sleepMinutes(currentGuard)._1 + (when.getMinutes - currStartSleep), sleepMinutes(currentGuard)._2)
    }
  }

  private val mostSleep: (Int, (Int, util.ArrayList[(Int, Int)])) = sleepMinutes.toList.sortBy(_._2._1).last
  println(mostSleep)

  val intervals: util.ArrayList[(Int, Int)] = mostSleep._2._2
  val minutesFreq = mutable.Map[Int, Int]().withDefault(_ => 0)
  intervals.forEach(new Consumer[(Int, Int)] {
    override def accept(t: (Int, Int)): Unit = for(i <- t._1 until t._2) {
      minutesFreq(i) += 1
    }
  })

  private val mostMinutes: (Int, Int) = minutesFreq.toList.sortBy(_._2).last
  println(mostMinutes)

  println(mostSleep._1 * mostMinutes._1)



  val sleepByGuard = mutable.Map[(Int, Int), Int]().withDefault(_ => 0)

  sleepMinutes.foreach({ case (guard, (_, sleeps)) =>
    sleeps.forEach(new Consumer[(Int, Int)] {
      override def accept(t: (Int, Int)): Unit = for (i <- t._1 until t._2) {
        sleepByGuard(guard, i) += 1
      }
    })
  })

  println(sleepByGuard.toList.sortBy(_._2).last)


}

