package day8

object day8 extends App {

  val input: List[Int] = day8Input.input.split(' ').map(Integer.parseInt).toList

  case class Node(childCount: Int, mdCount: Int, children: List[Node], md: List[Int])

  def parseNodes(inputs: List[Int]): (Node, List[Int]) = {
    val childCount = inputs.head
    val mdCount = inputs(1)
    var rest = inputs.drop(2)
    var children = List[Node]()
    for (i <- 1 to childCount) {
      val (node, remain) = parseNodes(rest)
      rest = remain
      children = children :+ node
    }
    var md = rest.take(mdCount)
    (Node(childCount, mdCount, children, md), rest.drop(mdCount))
  }

  def sumMd(node: Node): Int = node.md.sum + node.children.map(sumMd).sum

  private val topNode: Node = parseNodes(input)._1
  println(sumMd(topNode))




  def nodeValue(node: Node): Int ={
    if (node.children.isEmpty) node.md.sum
    else {
      node.md.map(idx =>
        if (idx == 0 || node.children.size < idx) 0
        else nodeValue(node.children(idx - 1))
      ).sum
    }
  }

  println(nodeValue(topNode))

}
