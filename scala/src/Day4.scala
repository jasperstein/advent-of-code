import scala.io.Source
object Day4 extends App {
  val input = Source.fromFile("Inputs/day4.txt").getLines

  val draws = input.next.split(",").map(Integer.parseInt)
  val boardStrings = input.grouped(6).toList.map(_.tail)
  val boards = boardStrings.map(_.map(_.trim.split("\\s+").map(Integer.parseInt)).flatten)
  
  case class HitCount(board: Seq[Int], rows: List[Int], cols: List[Int]) {
      private def hit(row: Int, col: Int): HitCount = HitCount(board, rows.updated(row, rows(row) + 1), cols.updated(col, cols(col) + 1))
      def hit(n: Int): HitCount = {
          val pos = board.indexOf(n)
          if (pos == -1 || hasWon) this else hit(pos / 5, pos % 5)
      }
      val hasWon = rows.contains(5) || cols.contains(5)
  }

  val initialHits = boards.map(board => HitCount(board, List.fill(5)(0), List.fill(5)(0)))
  
    val (drawn, hitCounts) = draws.foldLeft(List[Int]() -> initialHits) { 
        case (st@(stack, hits), next) => 
        if (hits.exists(_.hasWon)) st 
        else (next :: stack, hits.map(_.hit(next))) 
    }

    val winner = hitCounts.find(_.hasWon).get
    println(drawn)
    println(winner)  

    println(drawn.head * (winner.board.toList.diff(drawn).sum))
  
    val results = draws.foldLeft(initialHits.map(ih => List[Int]() -> ih)) { 
        case (hits, next) => 
            hits.map { case h@(stack, hc) => 
                if (hc.hasWon) h else (next :: stack, hc.hit(next))}
    }

    val loser = results.sortBy { case (stack, hc) => stack.length }.last

    println(loser)  

    println(loser._1.head * (loser._2.board.toList.diff(loser._1).sum))
  
}