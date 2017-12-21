package day21

object Grid extends App {

  val iteration0 = ".#./..#/###"

  def parseRules(input: String): Map[String, String] = input.lines.map(l => {
    val items: List[String] = l.split("=>").map(_.trim).toList
    items(0).replaceAll("/", "\n") -> items(1).replaceAll("/", "\n")
  }).toMap

  def flipV(config: String): String = config.lines.toList.reverse.mkString("\n")

  def rot(config: String) = flipV(config.lines.toList.map(_.toList).transpose.map(_.mkString("")).mkString("\n"))

  def applyRule(input: String, rules: Map[String, String]): String = {
    val allReps = List(input, flipV(input), rot(input), flipV(rot(input)), rot(rot(input)), flipV(rot(rot(input))), rot(rot(rot(input))), flipV(rot(rot(rot(input)))))
    val canonical = allReps.find(rules.isDefinedAt).get
    rules(canonical)
  }

  def transform(board: String): String = {
    val lines = board.lines.toList
    val groupSize = if (lines.head.length % 2 == 0) 2 else 3
    val subgrids = lines.map(l => l.grouped(groupSize).toList).grouped(groupSize).toList
    val sqs = subgrids.map(subgrid =>
      if (groupSize == 2) subgrid(0).zip(subgrid(1)).map(p => p._1 + "\n" + p._2)
      else subgrid(0).zip(subgrid(1).zip(subgrid(2))).map(p => p._1 + "\n" + p._2._1 + "\n" + p._2._2)
    )
    val replaced: List[List[String]] = sqs.map(_.map(applyRule(_, parseRules(Input.star1))))
    val result = replaced.map(_.map(_.lines.toList))
    val resultBoardLines = result.map(subgridLine =>
      if (groupSize == 2) {
        val line1 = subgridLine.map(sg => sg(0)).mkString
        val line2 = subgridLine.map(sg => sg(1)).mkString
        val line3 = subgridLine.map(sg => sg(2)).mkString
        List(line1, line2, line3).mkString("\n")
      } else {
        val line1 = subgridLine.map(sg => sg(0)).mkString
        val line2 = subgridLine.map(sg => sg(1)).mkString
        val line3 = subgridLine.map(sg => sg(2)).mkString
        val line4 = subgridLine.map(sg => sg(3)).mkString
        List(line1, line2, line3, line4).mkString("\n")
      }
    ).mkString("\n")
    resultBoardLines
  }

  private val result: String = transform(transform(transform(transform(transform(iteration0.replace("/", "\n"))))))
  println(result)
  println(result.count(_ == '#'))
}
