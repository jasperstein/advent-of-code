import scala.io.Source
object Day8 extends App {
    type Grid = Map[(Int, Int), Int]
    val input = Source.fromFile("inputs/day8.txt").getLines().toSeq
    val sample = Source.fromString("""30373
25512
65332
33549
35390""").getLines().toSeq
    def grid(source: Seq[String]) = source.zipWithIndex.flatMap{ 
        case (line, row) => line.toList.map(_.toString()).map(Integer.parseInt).zipWithIndex.map { 
            case (height, col) => (row, col) -> height
        } }.toMap
  
    def height(grid: Grid): Int = grid.keys.maxBy(_._1)._1
    def width(grid: Grid): Int = grid.keys.maxBy(_._2)._2

    def rowVisible(grid: Grid, range1: Range, range2: Range): Seq[(Int, Int)] = 
        range1.flatMap { row =>
            range2.foldLeft(Seq[(Int, Int)]() -> -1) { case ((trees, curr), col) => 
                if (grid(row, col) > curr) (trees :+ (row, col), grid(row, col))
                else (trees, curr)
            }._1
        }
    def colVisible(grid: Grid, range1: Range, range2: Range): Seq[(Int, Int)] = 
        range1.flatMap { col =>
            range2.foldLeft(Seq[(Int, Int)]() -> -1) { case ((trees, curr), row) => 
                if (grid(row, col) > curr) (trees :+ (row, col), grid(row, col))
                else (trees, curr)
            }._1
        }

    def visible(grid: Grid): Seq[(Int, Int)] = {
        val h = height(grid)
        val w = width(grid)
        val trees = rowVisible(grid, Range.inclusive(0, h), Range.inclusive(0, w)) ++
            rowVisible(grid, Range.inclusive(0, h), Range.inclusive(w, 0, -1)) ++
            colVisible(grid, Range.inclusive(0, w), Range.inclusive(0, h)) ++
            colVisible(grid, Range.inclusive(0, w), Range.inclusive(h, 0, -1))
        trees.distinct
    }

    def score(grid: Grid, range: Seq[(Int, Int)], height: Int): Int = 
        range.foldLeft(0) { case (count, pos) => if (grid(pos) >= height) 1 else count + 1 }

    def scenicScore(grid: Grid, row: Int, col: Int): Int = {
        val treeHeight = grid((row, col))
        score(grid, Range(0, col).map(c => (row, c)), treeHeight) *
        score(grid, Range(width(grid), col, -1).map(c => (row, c)), treeHeight) *
        score(grid, Range(0, row).map(r => (r, col)), treeHeight) *
        score(grid, Range(height(grid), row, -1).map(r => (r, col)), treeHeight)
    }
    println(visible(grid(sample)).size)
    println(visible(grid(input)).size)

    println(scenicScore(grid(sample), 1, 2))
    println(scenicScore(grid(sample), 3, 2))
    println(grid(input).keySet.map(pos => scenicScore(grid(input), pos._1, pos._2)).max)
}
