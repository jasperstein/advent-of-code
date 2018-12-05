package day5

import java.util

import scala.collection.mutable

object day5 extends App {

  val input = day5Input.input

  def areOppositePolarity(c1: Char, c2: Char): Boolean = {
    (c1.isLower && c2.isUpper && c2.toLower == c1) ||
    (c2.isLower && c1.isUpper && c1.toLower == c2)
  }

//  println(input.size)
  val stack = new util.Stack[Char]()
  for (c <- input) {
    if (stack.empty() || !areOppositePolarity(stack.peek(), c)) stack.push(c) else stack.pop()
  }
  println(stack.size())

  def areSpecificOppositePolarity(c: Char, c1: Char, c2: Char): Boolean = {
    (c1.isLower && c1==c && c2.isUpper && c2.toLower == c1) ||
      (c2.isLower && c2==c && c1.isUpper && c1.toLower == c2)
  }
  val theMap = mutable.Map[Char, Int]()
  for (test <- "abcdefghijklmnopqrstuvwxyz") {
    val stack = new util.Stack[Char]()
    for (c <- input) {
      if (c.toLower != test) {
        if (stack.empty() || !areOppositePolarity(stack.peek(), c)) stack.push(c) else stack.pop()
      }
    }
    theMap(test) = stack.size()
  }
  println(theMap.toList.sortBy(_._2))
}
