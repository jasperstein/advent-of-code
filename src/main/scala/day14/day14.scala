package day14

import scala.collection.mutable

object day14 extends App {

  val input = 760221
//  val input = 594142

  var elves= mutable.Map[Int, Node]()

  class Node(val score: Int, var next: Node)

  var lastRecipe = new Node(7, null)
  val recipeScores = new Node(3, lastRecipe)

  elves(0) = recipeScores
  elves(1) = recipeScores.next

  def skip(node: Node, n: Int): Node = if (n == 0) node else {
    val next = node.next
    skip(if(next == null) recipeScores else next, n-1)
  }
  private def addNewRecipes = {
    val newScores = (elves(0).score + elves(1).score).toString.map(c => Integer.parseInt(c.toString)).toList
    newScores.foreach(s => {
      val next = new Node(s, null)
      lastRecipe.next = next
      lastRecipe = next
    })
    elves(0) = skip(elves(0), 1 + elves(0).score)
    elves(1) = skip(elves(1), 1 + elves(1).score)
  }

  var candidate: Node = recipeScores
  var candidateIdx: Int = 0

  def skipTo7() {
    while (candidate.score != 7) {
      if (candidate.next == null) addNewRecipes
      candidate = candidate.next
      candidateIdx = candidateIdx + 1
    }
    addNewRecipes
    addNewRecipes
    addNewRecipes
    addNewRecipes
    addNewRecipes
    addNewRecipes
  }

  skipTo7()
  while (s"${candidate.score}${candidate.next.score}${candidate.next.next.score}${candidate.next.next.next.score}${candidate.next.next.next.next.score}${candidate.next.next.next.next.next.score}" != s"$input") {
//    println(candidateIdx + " -> " + s"${candidate.score}${candidate.next.score}${candidate.next.next.score}${candidate.next.next.next.score}${candidate.next.next.next.next.score}${candidate.next.next.next.next.next.score}")
    if (candidate.next == null) addNewRecipes
    candidate = candidate.next
    candidateIdx = candidateIdx + 1
    skipTo7()
  }
  println(candidateIdx)

}
