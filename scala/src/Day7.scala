import scala.annotation.tailrec
import scala.reflect.io.File

object Day7 extends App {
  private val input: List[String] = File("Inputs/day7.txt").lines().toList

  val regex = "(\\w+ \\w+) bags contain (\\d+ \\w+ \\w+|no other) bags?(, \\d+ \\w+ \\w+ bags?)*\\."

  input.foreach(l => if (!l.matches(regex)) println(l))
  println(input.size)
  println(input.count(_.matches(regex)))

  input.foreach(l => {
    val parts = l.split(" bags contain ")
    if (parts(1).contains("shiny gold")) println(parts(0))
  })

  case class ContainSpec(count: Int, bagType: String)

  val readSpec: String => (String, List[ContainSpec]) = line => {
    val parts = line.split(" bags contain ")
    val specs = parts(1).split(" ")
    if (specs.startsWith(Array("no", "other"))) (parts(0), List())
    else (parts(0), specs.grouped(4).toList.map(spec => ContainSpec(Integer.parseInt(spec(0)), spec(1) + " " + spec(2))))
  }

  val mapping: Map[String, List[ContainSpec]] = input.map(readSpec).toMap

  val splitted = input.map(_.split(" bags contain "))
  println(splitted.filter(_(1).contains("shiny gold")).map(_(0)))

  def findContainers(bagType: String) = mapping.filter(_._2.exists(_.bagType == bagType)).keySet
  var found = findContainers("shiny gold")
  var total = Set[String]()
  while (found.nonEmpty) {
    total = total ++ found
    println(found)
    found = found.flatMap(findContainers)
  }
  println(total)
  println(total.size)

  @tailrec
  def applyUntil[A](startWith: A)(stopWhen: A => Boolean)(f: A=>A): A =
    if (stopWhen(startWith)) startWith
    else applyUntil(f(startWith))(stopWhen)(f)

  private val theSeq: Seq[(Set[String], Set[String])] = Seq.unfold((Set("shiny gold"), Set[String]())) { case (curr, tot) =>
    val newItems = curr.flatMap(findContainers)
    if (newItems.isEmpty) None
    else Some((newItems, tot ++ newItems), (newItems, tot ++ newItems))
  }

  val all = applyUntil((Set("shiny gold"), Set[String]()))(_._1.isEmpty) { case (curr, tot) =>
    val newItems = curr.flatMap(findContainers)
    (newItems, tot++newItems)
  }
  println(all)
  println(all._2.size)

  println(theSeq)
  println(theSeq.last._2.size)

  def contents(bagType: String): Int = 1 + mapping(bagType).map(cs => cs.count * contents(cs.bagType)).sum

  println(contents("shiny gold"))

}
