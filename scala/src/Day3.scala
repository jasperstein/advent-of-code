import scala.io.Source
object Day3 extends App {
    val input = Source.fromFile("inputs/day3.txt").getLines().toSeq
    def rucksacks(line: String): (Seq[Char], Seq[Char]) = line.toList.splitAt(line.length()/2)
    def intersect(r1: Seq[Char], r2: Seq[Char]): Seq[Char] = r1.distinct.intersect(r2.distinct)
    def intersect1(rucksacks: (Seq[Char], Seq[Char])): Char = intersect(rucksacks._1, rucksacks._2).head
    val prios = (('a' to 'z') ++ ('A' to 'Z')).zipWithIndex.toMap
    println(input.map(rucksacks).map(intersect1).map(c => prios(c) + 1).sum)

    val groups = input.grouped(3).toList
    def badge(rucksacks: Seq[String]) = rucksacks(0).toList.distinct.intersect(rucksacks(1).toList.distinct).intersect(rucksacks(2).toList.distinct).head
    println(groups.map(badge).map(b => prios(b) + 1).sum)
}
