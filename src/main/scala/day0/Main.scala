import day0.{A, B, Down, Left, Puzzle, Right, Dummy, Up}
import day0.Types.{Position, State}

object Main extends App {
  def taxicab(pos1: Position, pos2: Position) = Math.abs(pos1.x - pos2.x) + Math.abs(pos1.y - pos2.y)

  val actions = Puzzle.puzzleInput.split(",").map(_.trim).map {
    case "Left" => Left
    case "Right" => Right
    case "Up" => Up
    case "Down" => Down
    case "A" => A
    case "B" => B
    case "Start" => Dummy
  }

  val startState = State(Position(0, 0), Map(A -> Set(), B -> Set()))
  val finalState = actions.foldLeft(startState)((st, i) => i.update(st))

  // 1. find marker furthest from origin
  val allMarkerPositions = finalState.markers(A) ++ finalState.markers(B)
  val origin = Position(0, 0)

  private val winner: Int = allMarkerPositions.foldLeft(-1)((currentMax, markerPos) => Math.max(currentMax, taxicab(origin, markerPos)))
  println(winner)

  // 2. find distinct markers most distant from each other

  //  println(finalState.markers(A).size)
  //  println(finalState.markers(B).size)
  //  -> brute force, there's only some 26000 of them:
  val allDistances = for {
    p1 <- finalState.markers(A)
    p2 <- finalState.markers(B)
  } yield taxicab(p1, p2)

  println(allDistances.foldLeft(-1)((currentMax, dist) => Math.max(currentMax, dist)))
}