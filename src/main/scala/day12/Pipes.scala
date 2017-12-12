package day12

import scala.collection.immutable

object Pipes extends App {

  def parseInput(input: String): List[(Int, List[Int])] = {
    input.lines.map(_.split("<->")).map(line =>
      Integer.parseInt(line(0).trim) -> line(1).split(",").map(_.trim).map(Integer.parseInt).toList).toList
  }

  def root(p: Int, roots: Array[Int]): Int = if (roots(p) == p) p else root(roots(p), roots)

  def group(pipes: List[(Int, List[Int])]): Array[Int] = {
    val roots = Array(pipes.indices: _*)
    pipes.foreach({
      case ((nodeNr, children)) => children.foreach(child => {
        val nodeRoot = root(nodeNr, roots)
        val childRoot = root(child, roots)
        roots(childRoot) = nodeRoot
      })
    })
    roots
  }

  val parsedExample = parseInput(Input.example)
  val exampleRoots = parsedExample.indices.map(root(_, group(parsedExample)))
  println(exampleRoots.count(_ == exampleRoots(0)))

  val parsedInput = parseInput(Input.star1)
  val realRoots = parsedInput.indices.map(root(_, group(parsedInput)))
  println(realRoots.count(_ == realRoots(0)))

}
