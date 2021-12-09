import scala.io.Source
object Day8 extends App {

  case class Digits(left: Seq[String], right: Seq[String]) {
    val n1 = left.find(_.length == 2).get
    val n4 = left.find(_.length == 4).get
    val n7 = left.find(_.length == 3).get
    val n8 = left.find(_.length == 7).get
    lazy val n9 = left.diff(Seq(n1, n4, n7, n8)).find(d => n4.split("").diff(d.split("")).isEmpty).get
    lazy val n3 = left.diff(Seq(n1, n4, n7, n8, n9)).find(d => d.split("").diff(n9.split("")).isEmpty && n1.split("").diff(d.split("")).isEmpty).get
    lazy val n5 = left.diff(Seq(n1, n4, n7, n8, n9, n3)).find(d => d.split("").diff(n9.split("")).isEmpty && n1.split("").diff(d.split("")).nonEmpty).get
    lazy val n6 = left.diff(Seq(n1, n4, n7, n8, n9, n3, n5)).find(d => n5.split("").diff(d.split("")).isEmpty).get
    lazy val n0 = left.diff(Seq(n1, n4, n7, n8, n9, n3, n5, n6)).find(d => d.length == 6).get
    lazy val n2 = left.diff(Seq(n1, n4, n7, n8, n9, n3, n5, n6, n0)).head

    private def parse(digit: String): Int = List(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9).indexOf(digit)
    def output: Int = Integer.parseInt(right.map(parse).mkString)
  }

  val input = Source.fromFile("Inputs/day8.txt").getLines.map(_.split("\\s+\\|\\s+")).map(arr => Digits(arr(0).split("\\s+"), arr(1).split("\\s+"))).toList

  val numbers1478 = input.map(_.right).map(_.count(ds => Seq(2,4,3,7).contains(ds.length)))

  println(numbers1478.sum)

  val normalize: String => String = s => s.split("").sorted.mkString
  val normalized = input.map(d => Digits(d.left.map(normalize), d.right.map(normalize)))

  println(normalized.map(_.output).sum)
}
