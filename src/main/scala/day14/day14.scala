package day14

import scala.collection.mutable

object day14 extends App {

  val input = 760221

  var elves= mutable.Map[Int, Node]()
//  elves(0) = 0
//  elves(1) = 1

  class Node(val score: Int, var next: Node)

  var lastRecipe = new Node(7, null)
  val recipeScores = new Node(3, lastRecipe)

  elves(0) = recipeScores
  elves(1) = recipeScores.next

//  var recipeScores = mutable.Queue[Int]()
//  recipeScores.enqueue(3)
//  recipeScores.enqueue(7)

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
    //    println(newScores)
    //    recipeScores.enqueue(newScores: _*)
    //    println(recipeScores)
    //    println(Math.floorMod(elves(0) + 1 + recipeScores(elves(0)), recipeScores.length))
    //    println(Math.floorMod(elves(1) + 1 + recipeScores(elves(1)), recipeScores.length))
    //    println(recipeScores.length)
    elves(0) = skip(elves(0), 1 + elves(0).score)
    elves(1) = skip(elves(1), 1 + elves(1).score)
  }

  var i = 0
  while (i < input + 10) {
    addNewRecipes

    i = i + 1
    if (i%10000 == 0) println(i)
  }



  var node = recipeScores
  i = 0
  while (i < input) {
//    print(node.score + " ")
    node = node.next
    i = i + 1
  }
  while (node != null) {
    print(node.score + " ")
    node = node.next
  }
//  println(recipeScores)
//  println(recipeScores)


}
