package day7
import scala.collection.immutable

object Hanoi extends App {

  var weights = Map[String, Int]() // we might just need these later...

  var parents = Map[String, String]()

  var children = Map[String, List[String]]()

  var subtreeWeights = Map[String, Int]()

  def parseWeight(spec: String): ((String, Int)) = {
    val split = spec.split("\\(").map(_.trim)
    val progName = split(0)
    val weight = Integer.parseInt(split(1).init)
    progName -> weight
  }

  def parseLine(line: String): Unit = {
    val elems = line.split("->").map(_.trim)
    val progWeight = parseWeight(elems(0))
    weights += progWeight
    val parent: String = progWeight._1
    if (elems.length > 1) {
      for (child <- elems(1).split(",").map(_.trim)) {
        parents += child -> parent
      }
    }
  }

  def root(input: String): String = {
    weights = Map()
    parents = Map()
    children = Map()
    subtreeWeights = Map()
    input.lines.foreach(parseLine)
    (weights.keySet diff parents.keySet).head
  }

  private val exampleRoot: String = root(Input.example1)
  println(exampleRoot)
  children = computeTree()
  calcSubtreeWeights(exampleRoot)

  private val star1root: String = root(Input.star1)
  children = computeTree()
  calcSubtreeWeights(star1root)

  def computeTree(): Map[String, List[String]] = {
    var tree = Map[String, List[String]](weights.keys.map(node => (node, List())).toSeq: _*)
    for ((child, parent) <- parents) {
      tree = tree + (parent -> (tree(parent) :+ child))
    }
    tree
  }

  def calcSubtreeWeights(node: String): Unit = {
    val nodeChildren = children(node)
    nodeChildren.foreach(calcSubtreeWeights)
    val childWeights: immutable.Seq[Int] = nodeChildren.map(subtreeWeights)
    if (!childWeights.forall(_ == childWeights.headOption.getOrElse(0))) {
      nodeChildren.zip(childWeights).foreach(println)
      nodeChildren.foreach(node => println(node + weights(node)))
    }
    subtreeWeights = subtreeWeights + (node -> (childWeights.sum + weights(node)))
  }

  // some manual mangling still needs to be done for star 2 ;-)

}
