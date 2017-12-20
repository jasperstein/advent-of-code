package day19

object Maze extends App {
  private val positions: Int = Input.star1.replaceAll("\\s", "").length
  val maxWidth = Input.star1.lines.map(_.length).max
  private val lines: List[(String, Int)] = Input.star1.lines.map(_.padTo(maxWidth, ' ')).toList.zipWithIndex

  def isCrossing(row: Int, col: Int): Boolean =
    !lines(row - 1)._1.charAt(col).isWhitespace &&
    !lines(row + 1)._1.charAt(col).isWhitespace &&
    !lines(row)._1.charAt(col - 1).isWhitespace &&
    !lines(row)._1.charAt(col + 1).isWhitespace

  val crossings = lines.tail.init.map({
    case ((l, row)) => l.zipWithIndex.map({
      case ((c, col)) => c match {
        case '-' => if (col == 0 || col == maxWidth - 2 || !isCrossing(row, col)) 0 else 1
        case '|' => if (col == 0 || col == maxWidth - 2 || !isCrossing(row, col)) 0 else 1
        case _ => 0
      }
    }).sum
  }).sum

  println(positions + crossings)
}
