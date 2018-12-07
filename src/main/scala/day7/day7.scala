package day7

import java.util

object day7 extends App {

  case class PrioSpec(c1: Char, c2: Char)

  def readPrio(s: String) = {
    val words = s.split(' ').toList
    PrioSpec(words(1)(0), words(7)(0))
  }

  val prios: List[PrioSpec] = day7Input.input.lines.map(readPrio).toList

  println(prios)

  //  var itemPrios = Map[Char, Int]().withDefaultValue(-1)

  def findPrio(c: Char, currPrios: List[PrioSpec]): Int = {
    var result = 0
    for (spec <- currPrios) {
      if (spec.c2 == c) {
        result = Math.max(result, findPrio(spec.c1, currPrios) + 1)
      }
    }
    //    itemPrios = itemPrios + (c -> result)
    result
  }

  def findFirst(letters: Array[Char], currPrios: List[PrioSpec]) = {
    letters.find(c => findPrio(c, currPrios) == 0)
  }

  var stack = new util.Stack[Char]

  var thePrios = prios

  var letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray

  while (thePrios.nonEmpty) {
    val firstChar = findFirst(letters, thePrios).get
    letters = letters.filterNot(_.equals(firstChar))
    stack.push(firstChar)
    if (thePrios.size == 1) {
      stack.push(thePrios.head.c2)
    }
    thePrios = thePrios.filterNot(spec => spec.c1 == firstChar)
    //    println(firstChar + thePrios.size)
    //    println(letters.toList)
  }

  println(stack)

  case class Busy(c: Char, timeLeft: Int)

  thePrios = prios
  var unstartedJobs = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray
  var workers = List(Busy('x', 0), Busy('x', 0), Busy('x', 0), Busy('x', 0), Busy('x', 0))

  def jobTime(c: Char) = c.toInt - 4

  println(jobTime('A'))

  var time = 0

  def tick(): Unit = {
    time = time + 1
    workers = workers.map(w => Busy(w.c, Math.max(w.timeLeft - 1, 0)))
  }

  def assignJobs(): Boolean = {
    if (workers.count(_.timeLeft <= 0) == 0) false else {
      findFirst(unstartedJobs, thePrios) match {
        case None => false
        case Some(job) =>
          unstartedJobs = unstartedJobs.filterNot(_ == job)
          workers = workers.tail :+ Busy(job, jobTime(job))
          workers.sortBy(_.timeLeft)
          true
      }
    }
  }

  def removeDoneJobPrios(): Unit = {
    workers.foreach { w =>
      if (w.timeLeft == 0) {
        thePrios = thePrios.filterNot(spec => spec.c1 == w.c)
      }
    }
  }

  while (unstartedJobs.nonEmpty) {
    removeDoneJobPrios()
    if (!assignJobs()) {
      println(time)
      println(workers)
      println(unstartedJobs.toList)
      tick()
    }
  }
}
